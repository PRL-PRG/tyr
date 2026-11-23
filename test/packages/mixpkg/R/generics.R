# An S3 generic and its methods. format_value is defined in a later file.
describe <- function(x, ...) UseMethod("describe")

describe.measure <- function(x, ...) {
  paste0(format_value(unclass(x)), " ", attr(x, "unit"))
}

describe.default <- function(x, ...) format_value(x)
