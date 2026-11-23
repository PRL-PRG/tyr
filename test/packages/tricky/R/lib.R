## Functions that are difficult to check at run time. tyr infers their types
## (inst/TYR), and contractr checks each call against these types.

## ---------------------------------------------------------------------------
## Overloads, and results whose type depends on the arguments
## ---------------------------------------------------------------------------

## Multiplies x by 2: a scalar gives a scalar, and a vector gives a vector.
scale2 <- function(x) x * 2

## The number of characters of a string, or else the length of x.
describe <- function(x) if (is.character(x)) nchar(x) else length(x)

## Converts a string to upper case, or rounds a number. R forces digits only
## for a number.
fmt <- function(x, digits) if (is.character(x)) toupper(x) else round(x, digits)

## Gives 1L for "a", "two" for "b", and NULL for the other keys.
lookup <- function(key) switch(key, a = 1L, b = "two")

## Gives y if x is NULL, and x if not (x %||% y). R forces y only if x is NULL.
either <- function(x, y) if (is.null(x)) y else x

## Concatenates x and y: the mode of the result is the union of their modes.
combine <- function(x, y) c(x, y)

## The maximum of x. For an empty x, max() gives -Inf, a double.
largest <- function(x) max(x)

## The number of characters of each element. For an empty xs, sapply() gives
## list().
nchars <- function(xs) sapply(xs, nchar)

## Gives x + y, or x if y is missing. y is optional, but has no default.
add <- function(x, y) if (missing(y)) x else x + y

## ---------------------------------------------------------------------------
## Laziness, and promises that escape the call
## ---------------------------------------------------------------------------

## Gives a if flag is TRUE, and b if not. R forces only one of them.
pick <- function(flag, a, b) if (flag) a else b

## Returns a closure that adds by to start. R forces start only when the
## closure runs.
make_counter <- function(start) function(by = 1L) start + by

## make_getter returns a closure that returns x. use_getter calls g and adds 1L.
make_getter <- function(x) function() x
use_getter <- function(g) g() + 1L

## Returns the environment of the call, which keeps the promise of x.
capture <- function(x) environment()

## try_value gives default if x fails. retry forces x two times: R evaluates a
## failed promise again.
try_value <- function(x, default) tryCatch(x, error = function(e) default)
retry <- function(x) {
    tryCatch(x, error = function(e) NULL)
    x
}

## Gives the first element of `...`. R forces only this element.
first_dot <- function(...) ..1

## Gives its `...` to combine().
forward <- function(...) combine(...)

## Returns 0L. The second on.exit() replaces the first, and its handler forces x.
log_twice <- function(x) {
    on.exit(cat("first handler\n"))
    on.exit(cat("second handler:", length(x), "\n"))
    0L
}

## Gives the code of the argument as a string, not its value.
label_of <- function(x) deparse(substitute(x))

## ---------------------------------------------------------------------------
## Values that go to other checked functions
## ---------------------------------------------------------------------------

## shout converts s to upper case. greet gives name to shout.
shout <- function(s) toupper(s)
greet <- function(name) shout(name)

## Calls shout on each element of xs, with vapply().
shout_all <- function(xs) vapply(xs, shout, "")

## The area of a shape: an S3 generic, with methods for squares and circles.
area <- function(shape) UseMethod("area")
area.square <- function(shape) shape$side * shape$side
area.circle <- function(shape) 3 * shape$r * shape$r
