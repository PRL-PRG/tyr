# The arguments come back in a list, named after their tags.
conv <- function(a, b)
    .C("convolve",
       as.double(a),
       as.integer(length(a)),
       as.double(b),
       as.integer(length(b)),
       ab = double(length(a) + length(b) - 1))$ab

# A logical comes back logical, of the same length: here a scalar.
positive <- function(x)
    .C("all_positive", as.double(x), length(x), ok = FALSE, NAOK = TRUE)$ok

upper <- function(s) .C("upper_first", s = as.character(s), as.integer(length(s)))$s

xor_bytes <- function(x, key)
    .C("xor_raw", x = as.raw(x), as.integer(length(x)), as.raw(key), PACKAGE = "dotc")$x

# The whole list.
both <- function(a) .C("all_positive", a = as.double(a), n = 1L, ok = TRUE)

# Not enough arguments: R refuses the call.
too_few <- function(a) .C("convolve", as.double(a))

# An integer where the routine takes a double*.
wrong_type <- function(n) .C("convolve", 1L, 1L, 1, 1L, double(1))

# Forwarded arguments cannot be matched with the parameters.
forward <- function(...) .C("convolve", ...)
