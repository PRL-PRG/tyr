# Inference fails on a recursive definition (it is not bound in its own
# body); the scripted model of replies.json gives it a signature.
fact <- function(n) if (n <= 1) 1 else n * fact(n - 1)

# Typed only if the type found for fact is bound.
fact5 <- function() fact(5)

# No signature fits: the scripted model gives up.
wrong <- function(x) toupper(list(x))

# Not tried: it fails on toupper first, but it also calls wrong, which stays
# unbound, so it fails whatever its signature.
uses_wrong <- function(x) { y <- toupper(list(x)) ; wrong(x) }

# Not tried: not a function definition.
not_a_function <- toupper(list(1))

# Not tried: it fails on toupper first, but it also calls rlang::is_true,
# which the prelude lacks. Its parameter f, called too, is local: not a reason.
uses_rlang <- function(x, f) { z <- f(x) ; y <- toupper(list(1)) ; rlang::is_true(z) }
