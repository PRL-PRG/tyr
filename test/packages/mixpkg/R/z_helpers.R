format_value <- function(x) paste(round(x, 2), collapse = ", ")

# Untypeable (a number plus a string), and its caller then reports it as an
# unbound variable.
bad_total <- function(x) sum(x) + "units"

report <- function(x) paste("total:", bad_total(x))
