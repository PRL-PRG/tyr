<!-- Copied from r-typing-pipeline/docs/annotation-guide.md, for the LLM fallback's "full" syntax. The tools it mentions (rsttq, roxygen tags, the run) are the pipeline's, not tyr's. -->
# Annotating an R package — the operational guide

`docs/type-system.md` says what the types *mean*. This says how to turn a
concrete R function into one.

Read this together with the type reference. When in doubt about whether a type
is well formed, or whether one type is included in another, **ask the engine**:

```sh
echo '(x: dbl, digits: int1?) -> CHR' | tools/rsttq   # -> ok  <type>
echo '"nopad" <= CHR1'                | tools/rsttq   # -> ok  true
```

---

## 1. Where the annotation goes

Directly above the function definition, as a roxygen `@type` tag:

```r
#' @type (bytes: dbl, style: CHR?) -> CHR
pretty_bytes <- function(bytes, style = c("default", "nopad", "6")) { ... }
```

This holds for **every** function definition in `R/`, not just exported ones and
not just top-level ones. Packages written in the "standalone" style keep most of
their code inside `local({ ... })` blocks:

```r
format_bytes <- local({
  #' @type (bytes: dbl, style: CHR?) -> CHR
  pretty_bytes <- function(bytes, style = c("default", "nopad", "6")) { ... }

  #' @type (bytes: dbl, smallest_unit: CHR1?) -> { amount: DBL, unit: CHR, negative: LGL }<data.frame>
  compute_bytes <- function(bytes, smallest_unit = "B") { ... }
  ...
})
```

Those inner functions get annotated exactly the same way. When the same function
is re-exported under another name (`pretty_bytes <- format_bytes$pretty_bytes`),
annotate the *definition*, not the alias.

## 2. The procedure for one function

1. **Formals first.** Copy the parameter names and their order out of the
   definition. They are part of the type and must match exactly.
2. **Read the guards.** `stopifnot`, `match.arg`, `inherits`, `if (!is.numeric(x)) stop(...)`
   are the author telling you the domain.
3. **Read the body** for what is actually done to each argument (arithmetic,
   indexing, `paste`, `[[`), and what shape the result has.
4. **Read the call sites**: `man/*.Rd` examples and `tests/`. If a documented
   example passes a value your type rejects, your type is wrong.
5. **Write the annotation**, then check it parses with `tools/rsttq`.

## 3. Choosing the type

Two rules, in order.

**1. If the code already checks, follow the check.** A guard is the author
writing the domain down; copy it. `stopifnot(is.numeric(x))` is `DBL | INT`,
`match.arg(style)` is `CHR?`, `inherits(x, "difftime")` is `<difftime, ...>`,
`length(x) == 1L` pins the length to `X1`. Do not paraphrase a guard into
something narrower than it lets through, and do not ignore it because a simpler
type was nearby.

**2. Otherwise, take the essential property.** Ask what has to be true for the
body to make sense, and stop there. Arithmetic on `x` → `dbl`. Indexing and
`paste` → `vec`. Straight into `.Call` → `DBL`. Detail the code does not depend
on is noise: a longer type is not a better one.

**Soundness beats both.** A type that rejects a call the function genuinely
serves is a bug, and the run will report it as a violation. When you cannot
decide between two candidates, take the wider one.

The simplest *spelling* also matters, in this order: `dbl` → `DBL` →
`dbl \ lgl`. Reach for the coercion-closed shorthand when R really is permissive;
use the exact name when the storage type is load-bearing, or when the shorthand
is empty of content (`chr` is the top vector type, so a character parameter is
`CHR`); use subtraction only for a set with no name of its own. Never write
`dbl \ int` — that is `DBL`.

### Recipes

| What the code says | Type |
|---|---|
| `stopifnot(is.character(x), length(x) == 1L)` | `CHR1` |
| `stopifnot(is.character(x))` | `CHR` |
| `stopifnot(is.numeric(x))` | `DBL \| INT` — that is exactly what `is.numeric` admits (rule 1) |
| arithmetic on `x`, no guard | `dbl` — the essential property, and `f(1L)`, `f(TRUE)` really work (rule 2) |
| `x` goes straight into `.Call`/`.C` | `DBL` — `REAL()` errors on an integer vector |
| `stopifnot(is.logical(x))` | `lgl`, or `^LGL1` for a flag that must be `TRUE`/`FALSE` |
| `inherits(x, "difftime")` | `<difftime, ...>`, or `dbl & <difftime, ...>` if it is also used numerically |
| `x` is an S3 object of a known class | `<cls, ...>` (with `...` — objects usually carry several classes) |
| `x` is a list of things | `{ T }`, or `{ a: T, b: U }` when the fields are fixed |
| `x` may be `NULL` | `T \| null` |
| parameter has a default | append `?` |
| parameter only read under `if (!missing(p))` | append `?` |
| `...` | `...: T` — `...: any` if anything goes |
| `f` is called as `f(x)` / `f(x, ...)` | `@(T) -> U` / `@(T, ...: any) -> U` — the call it receives, **never** `(x: T) -> U`; see the trap below |
| argument captured unevaluated (`substitute`, `enquo`, `enquos`) | `sym \| lang` for one, `expr` for a captured list — not checked at run time |
| `match.arg(style)` with `style = c("a", "b")` | `CHR?` — a character vector of *any* length, **not** `CHR1?`; see the trap below |

### Results

| What the function returns | Type |
|---|---|
| a formatted string per input element | `CHR` |
| a single string | `CHR1` |
| `invisible(NULL)`, `stopifnot(...)` | `null` |
| the input, transformed elementwise | tie it to the input with a variable: `(x: v('a)) -> v('a)` |
| a `data.frame` | `{ col: T, ... }<data.frame>` |
| an S3 object | `T & <cls, ...>` |
| something or `NULL` | `T \| null` |
| it always errors | `empty` |

### Length

`X1` means length exactly 1. Use it when the code guarantees a scalar
(`length(x) == 1L` guards, `sum()`, `paste(collapse = )`); use the plain form for
anything vectorised. If a function is vectorised *and* preserves length, say so
with two arrows:

```
((x: dbl1) -> CHR1) & ((x: dbl) -> CHR)
```

## 4. Traps

**Type the parameter as it is *received*, not as it is used.** Normalisation
that happens *inside* the function constrains nothing about the caller.
`match.arg(style)` is the classic case: after it runs, `style` is one of the
choices — but what arrives may be anything the caller passed, and, crucially,
one R idiom passes the *whole default vector*:

```r
time_ago <- function(date, format = c("default", "short", "terse")) {
  vague_dt(dt, format = format)      # `format` has NOT been narrowed yet
}
```

`vague_dt` really does receive a length-3 character vector here, so its `format`
is `CHR?` — a character vector of any length — not `CHR1?`. A `match.arg`
parameter keeps the length open unless you have checked that nothing forwards it
un-narrowed.

Do not reach for bare `chr?` here either: `chr` is the top vector type, so `chr?`
also admits numbers and would let `f(style = 42)` pass unnoticed.

**R's simplifying functions hand out lists.** `sapply()` and `apply()` return a
list whenever the pieces do not all have the same length, and a `character(0)`
piece is enough to trigger it. A parameter fed from one of them is
`CHR | {any}`, not `CHR` — prettyunits' `trim()` is exactly this
case, and it shows up in the run as a list arriving where a character vector was
declared. When a call site is `sapply(...)`/`apply(...)`/`lapply(...)`, check
what the inner function returns for the empty and the ragged case.

**Follow the forwarding chains.** Before settling a parameter's type, grep the
package for calls that pass it on. If `f` hands its parameter `p` to `g`, then
`g`'s type for that parameter must admit everything `f`'s type admits.
Annotating each function in isolation is how the mismatch above gets written.

**Do not widen a type to accommodate a call the package rejects.** Test suites
deliberately feed junk to check that a function errors
(`expect_snapshot(error = TRUE)`, `expect_error`). Those calls are outside the
domain by design; they will be reported as violations at run time, and that is
the correct outcome. Widening `dbl \ lgl` to `any` to silence them would destroy
the annotation's content.

**A callback is typed by its call, not by a signature.** A definition-site
signature `(x: T)` is generative: the engine makes each one a fresh type, so
`(f: (x: dbl) -> dbl)` admits no function at all, not even one annotated with
the identical text — and `check-forwarding.R` will report every hand-off of
such a parameter. Write how the function calls it:

```
#' @type (x: dbl, f: @(dbl) -> dbl) -> DBL
apply_to <- function(x, f) f(x)
```

`@(dbl) -> dbl` is "anything that can be called with one `dbl` and returns a
`dbl`"; a closure `function(x, y = 1)` fits it, one with two required
parameters does not.

**Omitted arguments are not their defaults.** The tooling records an argument the
caller did not supply as `absent`, not as the type of the default value, so `T?`
means exactly "may be omitted".

**`chr` is not "string".** `chr1 = vec1`; the whole coercion ladder lives below
`chr`. Write `CHR1` for a string, `CHR` for a character vector.

**`dbl` is not "double".** It contains `lgl`, `int` and `raw` — which is exactly
why it is the right type for an ordinary numeric parameter. Reach for `DBL` only
where the storage type is load-bearing: a value passed straight to `.Call`,
`typeof`-style reflection, a `vapply` template, or a result you want callers to
be able to rely on.

**`^` removes `NA`, and only from scalars.** `^DBL` is a parse error. Do not try
to say "this vector has no NAs".

**Bare types are already unconstrained.** `dbl` allows any attributes and any
class. Only write `<...>` / `with { ... }` when you want to *restrict*.

**Parameter names are semantics.** `f(digits = 2)` matches by name; renaming a
parameter in the type breaks every named call.

## 5. Precision policy

Write the most precise type that still admits every legitimate call. In order of
preference:

1. what the code's own checks admit, when there are any;
2. otherwise the essential property, in the simplest spelling that has it:
   the coercion-closed shorthand where R is permissive (`dbl`, `vec`), the exact
   name where the mode is load-bearing or the shorthand is empty of content
   (`DBL`, `CHR`, `CHR1`), a union where there are genuinely several shapes
   (`CHR | null`), subtraction only for a set with no name (`dbl \ lgl`);
3. `any` only when the parameter really is unconstrained (`identity`,
   `...: any` pass-throughs).

Precision is not the same as complexity. A type that takes three lines to read
is a worse annotation than a slightly wider one that is obvious — the checker
will tell you when a complicated type has a simple name, and you should take it.

Do not invent overloads that the code does not implement, and do not narrow a
parameter to the values the tests happen to use. The annotation is a claim about
the function, and the experiment's whole point is to find out where such claims
break.

## 6. Worked examples (prettyunits)

```r
#' @type (x: CHR1) -> null
assert_string <- function(x) {
  stopifnot(is.character(x), length(x) == 1L)
}

#' @type (x: <difftime, ...>) -> null
assert_diff_time <- function(x) {
  stopifnot(inherits(x, "difftime"))
}

#' @type (lhs: CHR1, rhs: vec) -> CHR
`%s%` <- function(lhs, rhs) {
  assert_string(lhs)
  do.call(sprintf, c(list(lhs), as.list(rhs)))
}

#' @type (bytes: DBL | INT, style: CHR?) -> CHR
pretty_bytes <- function(bytes, style = c("default", "nopad", "6")) { ... }

#' @type (bytes: DBL | INT, smallest_unit: CHR1?) -> { amount: DBL, unit: CHR, negative: LGL }<data.frame>
compute_bytes <- function(bytes, smallest_unit = "B") { ... }

#' @type (x: dbl, minval: dbl1?) -> CHR
pretty_p_value <- function(x, minval = 0.0001) { ... }

#' @type (x: dbl, digits: dbl?, sci_range: dbl1?, sci_sep: CHR1?) -> CHR
pretty_round <- function(x, digits = 0, sci_range = Inf, sci_sep = "e") { ... }

#' @type (date: <POSIXct, POSIXt, ...>, format: CHR?) -> CHR
time_ago <- function(date, format = c("default", "short", "terse")) { ... }
```

Every one of these follows a check in the code. `bytes` is `DBL | INT` because
`compute_bytes` guards with `stopifnot(is.numeric(bytes))`; `pretty_round`'s `x`
has no guard at all, so it gets the essential property, `dbl`. `style` and
`format` keep their length open (`CHR?`) because `match.arg` runs after the value
arrives and siblings forward the un-narrowed default, while `smallest_unit` is
`CHR1?` — its guard includes `length(smallest_unit) == 1`.

## 7. Done means

- every function definition in `R/` carries a `@type`;
- every annotation parses (`tools/rsttq`) and is as simple as it can be — the
  checker reports any type that has a shorter equivalent;
- every parameter that some other function in the package forwards to it is
  typed widely enough to accept what that caller may pass;
- parameter names and order match the formals, and every defaulted or
  `missing()`-guarded parameter is optional;
- `tools/check-annotations.R` is silent;
- the package still installs.
