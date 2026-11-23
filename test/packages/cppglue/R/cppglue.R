# Typed from the glue: init.c declares the routine at SEXP types only.
scale_by <- function(x, k) .Call(`_cppglue_scale_by`, x, k)

all_named <- function(x) .Call(`_cppglue_all_named`, x)
