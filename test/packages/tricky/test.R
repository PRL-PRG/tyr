## Runs the test cases of R/lib.R with the contracts of inst/TYR. Then it
## compares each outcome with the sensible outcome.
##
##   CONTRACTR_RSTT=/path/to/rstt Rscript test.R
##
## It uses contractr (tyr/contractr) and processx. It runs each call two times:
## in the instrumented package and in a plain copy of lib.R. The two runs must
## give the same result (transparency).
##
## An outcome lists the checked calls in the order they started. Then it gives
## the number of contract warnings, and E if the expression stopped with an
## R error:
##
##   greet[d=V r=-] shout[d=v r=-] W1 E
##
##   d=  the domain verdicts of the call, in the order of the records
##   r=  the return verdict of the call
##       ok, ? (unknown), V (root violation: raised), v (violation
##       downstream of another one: recorded, not raised), - (no verdict)
##       contractr examines each call independently. Thus, v occurs only in
##       the sensible outcomes.
##   r=V:caller  (sensible only) a violation of the type of the function that
##       the call returned. The blame goes to the caller of that function.

suppressMessages(library(contractr))
if (!nzchar(tyr_rstt_path())) stop("rstt not found: set CONTRACTR_RSTT")

here <- local({
    file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    if (length(file)) dirname(normalizePath(file)) else getwd()
})

## the instrumented package
lib <- tempfile("tricky-lib")
dir.create(lib)
status <- system2(file.path(R.home("bin"), "R"),
                  c("CMD", "INSTALL", "--no-test-load", "-l", shQuote(lib), shQuote(here)),
                  stdout = FALSE, stderr = FALSE)
if (status != 0L) stop("cannot install ", here)
library(tricky, lib.loc = lib)
insert_tyr_contract("tricky")

## the same functions, not instrumented
plain <- new.env(parent = globalenv())
sys.source(file.path(here, "R", "lib.R"), envir = plain)

is_contract_warning <- function(m) startsWith(m, "contract violation")

## Evaluate `expr` in a fresh environment whose parent is `parent`.
run <- function(expr, parent) {
    warnings <- character(0)
    error <- NA_character_
    value <- NULL
    output <- utils::capture.output(
        value <- tryCatch(
            withCallingHandlers(eval(expr, new.env(parent = parent)),
                                warning = function(w) {
                                    warnings <<- c(warnings, conditionMessage(w))
                                    invokeRestart("muffleWarning")
                                }),
            error = function(e) {
                error <<- conditionMessage(e)
                NULL
            }))
    list(value = value, output = output, warnings = warnings, error = error)
}

verdict_code <- function(verdict, origin) {
    ifelse(verdict == "ok", "ok",
           ifelse(verdict == "unknown", "?",
                  ifelse(is.na(origin), "V", "v")))
}

outcome <- function(rows, res) {
    calls <- character(0)
    for (id in sort(unique(rows$call_id))) {
        r <- rows[rows$call_id == id, ]
        d <- r[r$parameter_position == -2L, ]
        ret <- r[r$parameter_position == -1L, ]
        calls <- c(calls, sprintf(
            "%s[d=%s r=%s]", r$function_name[[1L]],
            if (nrow(d)) paste(verdict_code(d$verdict, d$origin), collapse = ">") else "-",
            if (nrow(ret)) verdict_code(ret$verdict, ret$origin) else "-"))
    }
    n <- sum(is_contract_warning(res$warnings))
    paste(c(calls, if (n) paste0("W", n), if (!is.na(res$error)) "E"), collapse = " ")
}

## what tells the instrumented run from the plain one, if anything
differences <- function(a, b) {
    d <- character(0)
    if (!identical(a$value, b$value)) d <- c(d, "value")
    if (!identical(a$output, b$output)) d <- c(d, "output")
    if (!identical(a$warnings[!is_contract_warning(a$warnings)], b$warnings)) d <- c(d, "warnings")
    if (!identical(a$error, b$error)) d <- c(d, "error")
    d
}

short <- function(type) gsub("<> with \\{  \\}", "", type)
show_value <- function(v) paste(utils::capture.output(str(v, give.attr = FALSE)), collapse = " ")

results <- list()
section <- function(title) cat("\n==== ", title, " ", strrep("=", max(0, 66 - nchar(title))), "\n", sep = "")

case <- function(id, code, sensible, why = "") {
    expr <- parse(text = code)[[1L]]
    plain_res <- run(expr, plain)
    clear_contracts()
    res <- run(expr, globalenv())
    rows <- get_contracts()
    clear_contracts()
    current <- outcome(rows, res)
    diff <- differences(res, plain_res)

    cat(sprintf("\n%s  %s\n", id, code))
    if (nzchar(why)) cat("    why     :", why, "\n")
    cat("    value   :", if (is.na(res$error)) show_value(res$value) else paste("error:", res$error), "\n")
    for (i in seq_len(nrow(rows))) {
        r <- rows[i, ]
        what <- if (r$parameter_position == -2L) "domain" else "return"
        cat(sprintf("    %-8s: %s %s %s%s\n", r$function_name, what, r$verdict,
                    short(r$actual_type),
                    if (!is.na(r$origin)) sprintf("  (origin: call %d)", r$origin) else ""))
    }
    for (w in res$warnings[is_contract_warning(res$warnings)])
        cat(paste0("    | ", strsplit(w, "\n", fixed = TRUE)[[1L]]), sep = "\n")
    cat("    current :", current, "\n")
    cat("    sensible:", sensible, if (identical(current, sensible)) "  [same]" else "  [DIFFERENT]", "\n")
    if (length(diff)) {
        cat("    NOT TRANSPARENT, differs in:", paste(diff, collapse = ", "), "\n")
        if ("value" %in% diff) cat("      plain value:", show_value(plain_res$value), "\n")
        if ("output" %in% diff) {
            cat("      plain output:       ", paste(plain_res$output, collapse = " | "), "\n")
            cat("      instrumented output:", paste(res$output, collapse = " | "), "\n")
        }
    }

    results[[length(results) + 1L]] <<- data.frame(
        id = id, call = code, current = current, sensible = sensible,
        same = identical(current, sensible), transparent = !length(diff),
        stringsAsFactors = FALSE)
}

set_severity("warning")

## --------------------------------------------------------------------------
section("Overloads, and results whose type depends on the arguments")

case("O1a", "scale2(1:3)", "scale2[d=ok r=ok]")
case("O1b", 'scale2("a")', "scale2[d=V r=-] W1 E",
     "outside the domain: caught when x is forced, before R fails")
case("O1c", 'scale2(factor("a"))', "scale2[d=ok r=V] W1",
     "Ops.factor returns a logical NA: the type of `*` ignores S3 dispatch")

case("O2a", 'describe(c("ab", "cde"))', "describe[d=ok r=ok]")
case("O2b", "describe(NULL)", "describe[d=ok r=ok]")

case("O3a", "fmt(3.14159, 2)", "fmt[d=ok r=ok]")
case("O3b", 'fmt("a")', "fmt[d=ok r=ok]",
     "digits is never forced when x is a string: R does not need it")
case("O3c", 'fmt(3.14, "2")', "fmt[d=V r=-] W1 E",
     "x fixes the overload, which then rejects digits when it is forced")
case("O3d", 'fmt("a", "b")', "fmt[d=ok r=ok]",
     "digits unused, so its type does not matter")

case("O4a", 'lookup("b")', "lookup[d=ok r=ok]",
     "the result type has literals: \"two\" | 1L | NULL")
case("O4b", 'lookup(c("a", "b"))', "lookup[d=V r=-] W1 E")

case("O5a", "either(NULL, 2)", "either[d=ok r=ok]")
case("O5b", 'either(1L, stop("never"))', "either[d=ok r=ok]",
     "y must not be forced")

case("O6a", 'combine(1L, "a")', "combine[d=ok r=ok]",
     "the mode variable is instantiated to int | chr")
case("O6b", "combine(list(1), 2)", "combine[d=ok r=ok]",
     "R gives list(1, 2)")

case("O7a", "largest(c(3L, 7L))", "largest[d=ok r=ok]")
case("O7b", "largest(integer(0))", "largest[d=ok r=V] W1",
     "max(integer(0)) is -Inf, a double, but the type says INT1")

case("O8a", 'nchars(c("a", "bb"))', "nchars[d=ok r=ok]")
case("O8b", "nchars(character(0))", "nchars[d=ok r=V] W1",
     "sapply() of an empty vector is list(), the type says INT")
case("O8c", 'nchars(list("a", 1:2))', "nchars[d=ok r=ok]",
     "only the { vec } -> any overload applies")

case("O9a", "add(1, 2)", "add[d=ok r=ok]")
case("O9b", "add(1)", "add[d=ok r=ok]",
     "y is optional: the body asks missing(y)")

## --------------------------------------------------------------------------
section("Laziness, and promises that escape the call")

case("E1a", 'pick(TRUE, 1L, stop("never"))', "pick[d=ok r=ok]")
case("E1b", 'pick(FALSE, stop("never"), "b")', "pick[d=ok r=ok]")
case("E1c", "pick(NA, 1, 2)", "pick[d=ok r=-] E",
     "the type allows any flag; R fails on NA")

case("E2a", "{ cnt <- make_counter(10L); cnt(); cnt(5L) }", "make_counter[d=ok r=ok]",
     "start is forced after make_counter returned")
case("E2b", '{ cnt <- make_counter("a"); cnt() }', "make_counter[d=V r=?] W1 E",
     "the violation can only be seen after the call returned")
case("E2c", '{ cnt <- make_counter(1L); cnt("x") }', "make_counter[d=ok r=V:caller] W1 E",
     "the returned closure is called outside the type make_counter gives it")

case("E3a", "use_getter(make_getter(41L))",
     "use_getter[d=ok r=ok] make_getter[d=ok r=ok]")
case("E3b", 'use_getter(make_getter("a"))',
     "use_getter[d=V r=-] make_getter[d=ok r=ok] W1 E",
     "g must return a number, and returns \"a\"")

case("E4a", '{ e <- capture(stop("boom")); "fine" }', "capture[d=ok r=ok]")
case("E4b", '{ e <- capture(1:3); get("x", envir = e) }', "capture[d=ok r=ok]",
     "forced after the call returned")
case("E4c", "{ e <- capture(1:3); substitute(x, e) }", "capture[d=ok r=ok]",
     "the code of the escaped promise")

case("E5a", 'try_value(stop("boom"), 0L)', "try_value[d=ok r=ok]")
case("E5b", '{ k <- 0L; retry({ k <- k + 1L; if (k == 1L) stop("flaky"); k }) }',
     "retry[d=ok r=ok]", "the first force fails, the second gives 2L")

case("E6a", 'first_dot(1, stop("never"))', "first_dot[d=ok r=ok]",
     "only ..1 is forced")
case("E6b", "first_dot()", "first_dot[d=ok r=-] E")

case("E7a", 'forward(1L, "a")', "forward[d=ok r=ok] combine[d=ok r=ok]")

case("E8a", "log_twice(1:3)", "log_twice[d=ok r=ok]",
     "only the second exit handler runs")

case("E9a", "label_of(my_variable)", "label_of[d=ok r=ok]",
     "deparse(substitute(x)) is \"my_variable\"")

## --------------------------------------------------------------------------
section("Values passed on to other checked functions")

case("C1a", 'greet("bob")', "greet[d=ok r=ok] shout[d=ok r=ok]")
case("C1b", "greet(mean)", "greet[d=V r=-] shout[d=v r=-] W1 E",
     "one violation, raised at the root; shout's is downstream")

case("C2a", 'shout_all(c("a", "b"))',
     "shout_all[d=ok r=ok] shout[d=ok r=ok] shout[d=ok r=ok]")
case("C2b", 'shout_all(list("a", 1:2))',
     "shout_all[d=V r=-] shout[d=ok r=ok] shout[d=ok r=ok] W1 E",
     "caught before vapply() fails on a result of length 2")
case("C2c", 'shout_all(list("a", mean))',
     "shout_all[d=V r=-] shout[d=ok r=ok] shout[d=v r=-] W1 E",
     "shout gets a part of the value that violated shout_all")

case("C3a", 'area(structure(list(side = 2), class = "square"))',
     "area[d=ok r=ok] area.square[d=ok r=ok]")
case("C3b", 'area(structure(list(side = "2"), class = "square"))',
     "area[d=V r=-] area.square[d=v r=-] W1 E",
     "the generic's type should be the one of its methods")

## --------------------------------------------------------------------------
section("Summary")

summary <- do.call(rbind, results)
width <- max(nchar(summary$current), nchar(summary$sensible))
for (i in seq_len(nrow(summary))) {
    s <- summary[i, ]
    cat(sprintf("%-4s %-3s %-2s %s\n", s$id, if (s$same) "" else "!=",
                if (s$transparent) "" else "T!", s$call))
    if (!s$same) cat(sprintf("              current : %s\n              sensible: %s\n",
                             s$current, s$sensible))
}
cat(sprintf("\n%d calls: %d as sensible, %d different, %d not transparent\n",
            nrow(summary), sum(summary$same), sum(!summary$same), sum(!summary$transparent)))
