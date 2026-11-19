## Functions that are difficult to check at run time. Each section is one
## test case. test.R calls these functions. tyr infers their types (inst/TYR),
## and contractr checks each call against these types.

## ---------------------------------------------------------------------------
## Overloads, and results whose type depends on the arguments
## ---------------------------------------------------------------------------

## O1. Arithmetic. A scalar gives a scalar and a vector gives a vector. An S3
## class can dispatch to an Ops method that returns a different type.
scale2 <- function(x) x * 2

## O2. The result type follows a type test on the argument.
describe <- function(x) if (is.character(x)) nchar(x) else length(x)

## O3. Correlated arguments. `digits` is important only when `x` is not a
## character string. R forces `digits` after `x`.
fmt <- function(x, digits) if (is.character(x)) toupper(x) else round(x, digits)

## O4. The result type depends on the value of the argument, not on its type.
lookup <- function(key) switch(key, a = 1L, b = "two")

## O5. `x %||% y`. The result is `y` only when `x` is NULL. R forces `y` only
## then.
either <- function(x, y) if (is.null(x)) y else x

## O6. Polymorphic. The mode of the result is the union of the argument modes.
combine <- function(x, y) c(x, y)

## O7. A base function whose result type depends on a value. max() of an
## empty vector is -Inf, a double, for all modes of its argument.
largest <- function(x) max(x)

## O8. sapply() simplifies to a vector, but for an empty input it returns an
## empty list.
nchars <- function(xs) sapply(xs, nchar)

## O9. Arity overloads. The second argument is optional because the body
## calls missing(), not because it has a default.
add <- function(x, y) if (missing(y)) x else x + y

## ---------------------------------------------------------------------------
## Laziness, and promises that escape the call
## ---------------------------------------------------------------------------

## E1. R forces only one of `a` and `b`.
pick <- function(flag, a, b) if (flag) a else b

## E2. R forces `start` only in a call of the returned closure, after
## make_counter() returned.
make_counter <- function(start) function(by = 1L) start + by

## E3. The promise escapes in a closure. Another checked function forces it.
## The type of that function also gives the return type of the closure.
make_getter <- function(x) function() x
use_getter <- function(g) g() + 1L

## E4. The promise escapes in the environment of the call.
capture <- function(x) environment()

## E5. tryCatch() catches the error of the forced argument. R evaluates a
## failed promise again when R forces it again.
try_value <- function(x, default) tryCatch(x, error = function(e) default)
retry <- function(x) {
    tryCatch(x, error = function(e) NULL)
    x
}

## E6. R forces only the first element of `...`.
first_dot <- function(...) ..1

## E7. `...` goes to another checked function.
forward <- function(...) combine(...)

## E8. An exit handler forces the argument. The second on.exit() replaces the
## first one.
log_twice <- function(x) {
    on.exit(cat("first handler\n"))
    on.exit(cat("second handler:", length(x), "\n"))
    0L
}

## E9. The body uses the code of the argument, not its value.
label_of <- function(x) deparse(substitute(x))

## ---------------------------------------------------------------------------
## Values that go to other checked functions
## ---------------------------------------------------------------------------

## C1. A promise goes down a chain of checked functions.
shout <- function(s) toupper(s)
greet <- function(name) shout(name)

## C2. The same, but a base function (vapply()) gives each element to shout().
shout_all <- function(xs) vapply(xs, shout, "")

## C3. S3 dispatch. The method gets the promises of the generic.
area <- function(shape) UseMethod("area")
area.square <- function(shape) shape$side * shape$side
area.circle <- function(shape) 3 * shape$r * shape$r
