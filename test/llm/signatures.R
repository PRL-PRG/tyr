double_it <- function(x) x * 2

fact <- function(n) if (n <= 1) 1 else n * fact(n - 1)

first <- function(x) x[[1]]

shout <- function(s) paste0(toupper(s), "!")

wrong <- function(x) toupper(list(x))

pick <- function(flag, a, b) if (flag) a else b

count_na <- function(v) sum(is.na(v))

mutual_a <- function(x) mutual_b(x)
mutual_b <- function(x) mutual_a(x)

scale_by <- function(x, by = 2, ...) x * by

name_or <- function(x) if (is.null(x)) "none" else paste(x)

field_or <- function(s) if (is.null(s$a)) "none" else s$a

"[.mybox" <- function(x, i) unclass(x)[i]

`%+%` <- function(a, b) paste0(a, b)

ones <- function(...) array(1, dim = c(...))
