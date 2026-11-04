# Typed from the types array of its registration: no prototype.
scale <- function(x, a)
  .Fortran(F_scale, x = as.double(x), as.integer(length(x)), as.double(a), ok = FALSE)$x

scaled_ok <- function(x)
  .Fortran(F_scale, as.double(x), length(x), 2, ok = FALSE)$ok

# Registered as LGLSXP: an integer is refused, unlike for an int* prototype.
wrong_flag <- function(x)
  .Fortran(F_scale, as.double(x), length(x), 2, ok = 0L)

# Registered without types: typed from its prototype, whose int* also takes a
# logical.
count <- function(x)
  .Fortran(F_count, as.double(x), as.integer(length(x)), k = integer(1))$k

count_lgl <- function(x)
  .Fortran(F_count, as.double(x), TRUE, k = FALSE)$k

# Not registered: R looks the symbol dist_ up.
dist <- function(x) .Fortran("dist", x = as.double(x), length(x))$x

# A .Call routine registered under another name than its C function.
len <- function(x) .Call(F_pkg_len, x)
