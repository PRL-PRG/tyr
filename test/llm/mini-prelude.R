# The operators pkg/R/fact.R uses, on scalars: enough for the end-to-end test,
# whose output then does not list the whole base prelude.
#| base::(*) : (e1: dbl1, e2: dbl1) -> dbl1
#| base::(-) : (e1: dbl1, e2: dbl1) -> dbl1
#| base::(<=) : (e1: dbl1, e2: dbl1) -> LGL1
#| base::toupper : (x: CHR) -> CHR
#| base::list : (...: any) -> { any }
