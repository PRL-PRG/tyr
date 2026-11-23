# R wrappers of the native functions.
scale_by <- function(x, k = 1) .Call(C_scale_by, as.double(x), k)

measure <- function(x, unit = "m") .Call(C_with_unit, scale_by(x), unit)

count_missing <- function(x) .Call(C_count_na, as.double(x))
