<!-- Copied from r-typing-pipeline/docs/type-system.md, for the LLM fallback's "full" syntax. The tools it mentions (rsttq, roxygen tags, the run) are the pipeline's, not tyr's. -->
# The R set-theoretic type system — reference

This is the reference an annotator (human or AI) needs in order to write types
for R functions. It is self-contained: everything needed to write a correct
annotation is here.

Normative sources, if something here is unclear or looks wrong: `resources/syntax.md`
(grammar summary), the RSTT README in `external/rstt/README.md` (the type algebra
as implemented), `external/Rsem/tests/00_defs.r` (base-R signatures written by the type
system's authors — the best style guide there is), and `external/Rsem/types.ref` (what
inferred types print like).

The engine is queryable: `tools/rsttq` feeds `;;`-terminated commands to the
`rstt` REPL, so `dbl <= any ;;` prints `true` and a malformed type prints an
error. Use it. Never guess whether a type parses.

---

## 1. A type is a set of R values

Every type denotes a set of values, and *subtyping is set inclusion*: `T1 <= T2`
holds exactly when every value in `T1` is also in `T2`. There is no nominal
declaration, no structural-vs-nominal distinction, no variance rules to
memorise — everything follows from the sets.

That is why the algebra below has unions, intersections and negations at every
level: they mean what they mean in set theory.

## 2. The boolean algebra

| Syntax | Meaning |
|---|---|
| `any` | every value |
| `empty` | no value (bottom) |
| `T1 \| T2` | union |
| `T1 & T2` | intersection |
| `T1 \ T2` | difference |
| `~T` | negation, i.e. `any \ T` |
| `(T)` | grouping |

## 3. Atomic vectors

R's atomic vectors are described by two families of shorthands. **The
difference between them is the single most common source of mistakes.**

| Syntax | Meaning |
|---|---|
| `LGL`, `INT`, `DBL`, `CLX`, `CHR`, `RAW` | a vector of *exactly* that mode, any length |
| `lgl`, `int`, `dbl`, `clx`, `chr`, `raw` | that mode **plus every mode below it in R's coercion ladder** |
| `vec` | any atomic vector (lists are *not* atomic vectors) |
| `X1` (`INT1`, `dbl1`, `vec1`, …) | the same, restricted to length exactly 1 |
| `NUM1` | a whole number, integer or double, length 1: `42L` and `42.` but not `DBL1` as a whole. There is no `NUM`: element-level values are only allowed on scalars (§4). **Do not use it for a parameter**: the run records a double's mode, not its value (§12), so every double argument (`^DBL1`) refutes it. Write `dbl1` (or `int1`) |

The coercion ladder is R's own: `raw < lgl < int < dbl < clx`, with `chr` above
everything. So:

- `dbl` = "a double vector, *or anything R would silently coerce to one*" —
  logical, integer and raw vectors are all in `dbl`. This is usually what you
  want for a numeric parameter, because `f(1L)` and `f(TRUE)` really do work.
- `DBL` = "actually a double vector". `1L` is **not** in `DBL`.
- `chr` includes *every* atomic vector (everything coerces to character). It is
  literally the top vector type: `chr1 = vec1` is `true`.

**Prefer the simplest type that is true.** In order:

1. **`dbl`, `int`, `lgl`, `raw`** — the coercion-closed shorthand. This is what
   an ordinary R parameter accepts, because R coerces: a function that does
   arithmetic really does work on `1L` and `TRUE`.
2. **`DBL`, `INT`, `CHR`, `CHR1`, `LGL1`** — the exact name. Use it when the
   storage type is load-bearing (see §3.1), and for character, where the
   coercion-closed name is useless: `chr1 = vec1`, so `chr` says nothing.
3. **`dbl \ lgl`, `chr \ clx`** — subtraction. Last resort, for a set that has
   no name of its own. `dbl \ int` is just a long way to write `DBL`; don't.

Which of the three you want follows from the code, not from taste: if the
function already checks its argument, the type follows the check
(`stopifnot(is.numeric(x))` really is `DBL | INT`); if it does not, take the
essential property the body depends on and stop there. A more complicated type
is not a better one, and a type narrower than what the function accepts is a
bug. `docs/annotation-guide.md` §3 has the full rule.

### 3.1 When the exact mode is load-bearing

Only a few situations make R itself care about the storage type:

- **the C boundary**: `REAL(x)` inside a `.Call` errors on an integer or logical
  vector (`REAL() can only be applied to a 'numeric', not a 'integer'`), so a
  parameter passed straight to compiled code is `DBL`;
- **reflection**: `typeof`, `storage.mode`, `is.double`, `identical(1L, 1)`;
- **`vapply`'s template**: `vapply(c(1,2), identity, integer(1))` is an error;
- **results**: `5L %/% 2L` is integer, `5 %/% 2` is double — a function that
  returns a double vector should say `DBL`, otherwise nothing downstream can
  rely on it.

Reflection is also why the exact family has to exist at all. Typing `typeof`
with coercion-closed modes cannot work: `INT1 <= dbl` is true, so an integer
argument would have to satisfy both overloads and return
`"double" & "integer"` — which is `empty`. With exact modes the domains are
disjoint:

```
## typeof : ( x: DBL ) -> "double"
## typeof : ( x: INT ) -> "integer"
```

```
> RAW | LGL ;;          # -> lgl
> vec \ CHR ;;          # -> clx
> INT1 <= dbl ;;        # -> true
> INT1 <= DBL ;;        # -> false
> chr1 = vec1 ;;        # -> true   (!)
> RAW1 <= lgl1 ;;       # -> true
> dbl \ lgl ;;          # -> DBL | INT
> chr1 \ clx1 = CHR1 ;; # -> true
```

Underneath the shorthands is the general constructor `v(P)` (any length) and
`v1(P)` (length 1), where `P` is a *primitive value* expression:

- literals: `42L` (integer), `42.` (double), `42` (integer **or** double),
  `"txt"` (string), `tt` / `ff` (the two logicals);
- intervals: `(1L..10L)`, `1L..`, `..10L`, `(..)`;
- the same `| & \ ~` algebra as types, plus `^P` (see NA below);
- primitive variables `'a`, to relate an argument's element type to the result's.

A literal used as a type is the length-1 vector holding exactly that value:
`42L` means `v1(^42L)`, `"nopad"` means `v1(^"nopad")`.

```
> 42L = v1(^42L) ;;     # -> true
> v('a) -> v1('a) ;;    # a function from any atomic vector to a scalar of the same mode
```

## 4. `NA` and `^`

**Every primitive domain contains `NA` unless you subtract it.** `^P` is `P`
without `NA`.

*`^` only applies to scalars.* The current engine rejects `^DBL`, `^dbl` and
`v(^DBL)` with `Non-scalar vectors cannot feature singleton types`; only the
length-1 forms work:

- `^DBL1`, `^dbl1` (= `v1(^dbl)`), `^LGL1`, `^CHR1` — fine.
- `^LGL1` is exactly `tt | ff`. (`^lgl1` is wider: `lgl` also contains `raw`.)
- `DBL` — a double vector that may contain `NA`s, and there is **no** way to say
  "a double vector of any length with no `NA`".
- Literals already exclude `NA`: `tt`, `42.`, `"txt"` (`"txt" = v1(^"txt")`).

Note this is the *opposite* of contractr's old tastr convention, where a leading
`^` flagged the presence of `NA`.

Annotation rule of thumb: use the plain type (`dbl`, `chr`) for parameters,
because callers do pass `NA`s; reach for `^x1` only when a scalar result
genuinely cannot be `NA`.

## 5. Lists

`{ ... }` describes `list()` values — named and/or unnamed elements.

| Syntax | Meaning |
|---|---|
| `{ a: T, b: U }` | *closed*: exactly the fields `a` and `b` |
| `{ a: T, U }` | *open*: field `a`, and every other element has type `U` |
| `{ U }` | homogeneous list of any number of `U` elements |
| `{ any }` | the top list type — "some list" |
| `{ a: T? }` | field `a` may be absent |
| `{ a: absent }` | field `a` must **not** be present |
| ``{ a: T, `r }`` | field `a`, plus a row variable capturing all other fields unchanged |

```
> { a:42L, b:ff, int } <= { b:ff, int } ;;      # -> true
> { a:absent, `r } -> { a:v1(^42L), `r } ;;     # "sets $a to 42, leaves the rest alone"
```

Row variables are how you type list transformers precisely: the same `` `r ``
appearing in the parameter and the result says "every other field comes out
exactly as it went in".

## 6. Attributes and classes

Most R values carry attributes, and a `class` attribute drives S3 dispatch.

`<...>` constrains the **classes**:

| Syntax | Meaning |
|---|---|
| `<c1>` | has exactly class `c1` and nothing else |
| `<c1, ...>` | has at least class `c1`; other classes unspecified |
| `<~c1, ...>` | does not have class `c1` |
| `<?c1, ...>` | may or may not have `c1` |
| `<>` | carries no class at all |

`with { ... }` constrains the **other attributes**, as a list type:

```
> int with { dim:int } ;;
> int <= int with { any } ;;    # -> true
```

**A bare type is unconstrained**: `dbl` already means "a double vector with any
attributes and any classes". You only need `<...>` / `with` to *restrict*:
`dbl<> with {}` is "a bare double vector, no class, no attributes", and
`dbl1 & <difftime, ...>` is "a scalar double carrying (at least) class
`difftime`".

Two spellings you will use constantly:

- argument that must be an S3 object: `dbl & <difftime, ...>`, `{any} & <data.frame, ...>`;
- value known to be attribute-free (mostly produced by inference, rarely written
  by hand): `42.<> with {}`.

## 7. Functions

A function type is `Params -> Result`.

`Params` is a **definition-site** signature: a parenthesised list of *named*
formal parameters in declaration order, optionally ending in `...`.

```
(x: dbl) -> dbl
(x: dbl, digits: int1?) -> chr
(...: any, sep: chr1?, collapse: chr1?) -> CHR1
() -> null
```

- Parameter names matter: they are how a call `f(digits = 2)` is matched, and
  they must be exactly the R function's formals, in the same order.
- `T?` marks an **optional** parameter — the caller may omit it. Every parameter
  with a default value in R, and every parameter guarded by `missing()`, is
  optional.
- `...: T` types R's ellipsis: every extra argument must be in `T`.
- `absent` as a parameter type means the parameter must *not* be supplied.

`@(T1, T2, name: T3)` is the matching **call-site** signature (positional
arguments first, then named). You rarely write one by hand; it appears when
typing functions that forward `...`, and it is how "does this call match this
signature?" is decided — which is exactly the check this experiment performs
offline on each recorded call:

```
> @(DBL, "nopad")        <= (bytes: dbl, style: CHR1?) ;;   # -> true
> @(DBL, style: "nopad") <= (bytes: dbl, style: CHR1?) ;;   # -> true
> @(DBL, style: 42.)     <= (bytes: dbl, style: CHR1?) ;;   # -> false
> @(DBL, DBL)            <= (bytes: dbl, style: CHR1?) ;;   # -> false
> @(int)                 <= (a: int, b: int1?) ;;           # -> true (b omitted)
> @()                    <= (a: int, b: int1?) ;;           # -> false (a required)
```

`->` allows the function value itself to carry attributes/classes; `-->` is the
bare arrow without that, used for C-level functions. Use `->`.

### Parameters that take a function

**A definition-site signature is generative.** The engine gives every
`(x: …)` it builds a fresh identity, so two signatures written separately are
different types even when the text is the same:

```
> (x: int) <= (x: int) ;;                           # -> false
> ((x: dbl) -> dbl) <= ((x: dbl) -> dbl) ;;         # -> false
```

That is right for declaring a function, and fatal for describing one that is
passed in: `(f: (x: dbl) -> dbl) -> dbl` can never be satisfied, because no
argument's type is a subtype of that fresh signature. Type a callback by how it
is **called**, with a call-site domain `@(...)`:

```
> (f: @(dbl) -> dbl) -> dbl ;;                      # f is called with one dbl
> ((x: int) -> int) <= (@(int) -> int) ;;           # -> true
> ((x: any, y: any?) -> any) <= (@(dbl) -> any) ;;  # -> true (y may be omitted)
> ((x: dbl, y: dbl) -> any) <= (@(dbl) -> any) ;;   # -> false (y is required)
> (@(dbl) -> dbl) <= (@(int) -> dbl) ;;             # -> true (contravariant)
```

This is how Rsem types `lapply`'s `FUN` (§8). Write the call the function
actually makes: positional arguments in order, named ones by name, and `...: T`
when it forwards its own dots to the callback.

The runtime cannot see a closure's type. It records every function value as
`(...: any) -> any` (§12), and the report classes those observations as
`opaque` rather than checking them, so a callback's type is a claim the run
neither confirms nor refutes.

### Overloads are intersections

An intersection of arrows is a function that satisfies *all* of them — which is
exactly what an overloaded/dispatching R function does:

```
(x: DBL) -> DBL & (x: CHR) -> CHR
```

Write one arrow per genuinely different behaviour. `external/Rsem/tests/00_defs.r` types
base R this way:

```
## (+) : ( x:v('a)&dbl, y:v('a)&dbl) -> v('a)&dbl
## (+) : ( x:dbl1, y:dbl1) -> dbl1
## typeof : ( x: DBL ) -> "double"
## typeof : ( x: CHR ) -> "character"
## isTrue : (x:tt , na: lgl1?) -> tt
## isTrue : (x:ff , na: lgl1?) -> ff
```

Note the pattern in `(+)`: a scalar overload (`dbl1 -> dbl1`) *in addition to*
the vector one, because "scalar in, scalar out" is information the vector arrow
loses. Do the same for vectorised R functions whose result length follows the
input length.

## 8. Type variables

- `'a`, `'x`, … (lowercase first letter) — **monomorphic** variables: they stand
  for one unknown type and are printed back as-is.
- `'A`, `'X`, … (uppercase) — **polymorphic**: the solver may instantiate them.
- `` `r `` / `` `R `` — the same distinction for **row** variables (used inside
  list, argument and class types).

Use a variable when the result type is *tied* to an argument type:

```
## identity : ( x:'a ) -> 'a
## lapply : (X:{'a}, FUN:@('a, X:absent, FUN:absent, ...:`r) -> 'b, ...:`r) -> {'b}
```

## 9. Other SEXP types

`null`, `env`, `sym`, `lang`, `expr`, `externalptr(T)`, and the tuple constructor
`[T1, T2]` (not an R value — used for C-level data). `null` is its own type:
`NULL` is not a vector and is not in `any`-vector shorthands, so a function that
may return `NULL` needs `| null` in its result type.

`sym` is a symbol (`quote(x)`), `lang` a call (`quote(f(x))`, also formulas and
quosures, which are calls with a class). `expr` is a list whose elements are
each a symbol or a call, named or not — the shape of captured, unevaluated
arguments (`rlang::exprs(...)`, `rlang::enquos(...)`, `as.list(substitute(...))`).
It is what non-standard-evaluation functions such as `dplyr::select` work on:

```
> { a: sym, b: lang } <= expr ;;                    # -> true
> { a: lang<quosure, formula, ...> } <= expr ;;     # -> true (quosures are calls)
> { a: sym, b: dbl } <= expr ;;                     # -> false
> sym <= expr ;;                                    # -> false (a list, not one expression)
```

The runtime records an argument only when its promise is forced, and an
argument captured with `substitute`/`enquo` is never forced, so NSE arguments
produce no observation: their types are not checked by the run.

## 10. Recursive types

`T where x = Tx [and y = Ty]` binds recursive variables:

```
> a where a = { a } | dbl ;;
```

Needed for genuinely recursive structures (trees, nested lists of unbounded
depth).

## 11. How annotations are written in R sources

Two carriers, same type language:

1. **roxygen tag** (what the annotation agent writes, and what `contractr`
   consumes):

   ```r
   #' @type (bytes: dbl, style: chr1?) -> CHR
   pretty_bytes <- function(bytes, style = c("default", "nopad", "6")) { ... }
   ```

2. **Rsem comment form** (what `rsem` reads):

   ```r
   ## pretty_bytes : (bytes: dbl, style: chr1?) -> CHR
   ## annot_loop::acc : dbl          # a variable local to a top-level function
   ## new area.square                # declares an S3 method overload
   ## area.square : ( shape:dbl1 ) -> chr1
   ```

   Repeating a `##` line for the same name adds an overload (an intersection).
   `## new f.cls` removes class `cls` from the domain of the previous overloads
   of `f`, so S3 dispatch is modelled exactly.

You only write the first. The pipeline collects the `@type` tags
(`tools/gen-typedecl.R`) into `TYPEDECLARATION.typr` in the run directory, one
`name : signature` per line without the `##`, and that is the file contractr
imports.

## 12. Quick reference: R value → its most precise type

These are the types the runtime records for observed values (contractr's
`tyr_infer_type`), and there is only one way it records them. They use the exact
names on purpose: an observation is a measurement of one concrete value, so it
states the mode that value really had. `DBL <= dbl` holds, so it still satisfies
the permissive annotation an author would normally write.

| R value | type |
|---|---|
| `42` | `^DBL1<> with {  }` |
| `42L` | `^INT1<> with {  }` |
| `"nopad"` | `^CHR1<> with {  }` |
| `TRUE` | `^LGL1<> with {  }` |
| `1i` | `^CLX1<> with {  }` |
| `NA` | `LGL1<> with {  }` (`NA` is a logical) |
| `NA_real_` | `DBL1<> with {  }` |
| `c(1, 2, 3)` | `DBL<> with {  }` |
| `c(1, NA)` | `DBL<> with {  }` |
| `character(0)` | `CHR<> with {  }` |
| `matrix(1:4, 2)` | `INT<> with { dim: INT<> with {  } }` |
| `factor("a")` | `^INT1<factor> with { levels: ^CHR1<> with {  } }` |
| `as.difftime(1, units = "secs")` | `^DBL1<difftime> with { units: ^CHR1<> with {  } }` |
| `NULL` | `null` |
| `list()` | `{  }<> with {  }` |
| `list(a = 1, b = "x")` | `{ a: ^DBL1<> with {  }, b: ^CHR1<> with {  } }<> with {  }` |
| `data.frame(x = 1:2)` | `{ x: INT<> with {  } }<data.frame> with { row.names: INT<> with {  } }` |
| `function(x) x`, `sum` | `(...: any) -> any` (every function value) |
| `new.env()` | `env` |
| `quote(x)` | `sym` |
| `quote(f(x))` | `lang<> with {  }` |
| `~x` | `lang<formula> with { .Environment: env }` |
| an omitted argument | `absent` |
| an argument never forced | nothing: no observation is recorded |

Scalars are recorded by mode, not by value: `42` and `43` have the same type
(`^DBL1`), and so the literal types an annotation may use (`42.`, `"nopad"`,
`tt`) are never what a value is observed as: `^CHR1 <= "nopad"` is false. The
report does not count that as a violation; it classes it `literal`, meaning
the run cannot tell. So a literal in an argument type is a claim the run never
checks. Use literals where the code really distinguishes values (overload
domains, results), not to list the values the tests happen to pass. The same
holds for any other value-level refinement — `NUM1` (whole numbers), an
interval such as `(1L..10L)` — except that the report has no name for those
and counts them as violations: `^DBL1 <= NUM1` is false for `20` as much as for
`2.5`. A scalar `NA` is recorded without the `^` (`DBL1`), which is why `^dbl1`
refutes it.

Vectors of length ≠ 1 carry no element-level detail: the engine allows neither
singletons nor `^` there, so `c(1, 2)` and `c(1, NA)` have the same type.

A list's `names` and a data frame's `names` are not recorded as attributes —
they are the field labels. A function value is always `(...: any) -> any`, a
placeholder: the report classes those observations as `opaque` (§7).

## 13. Mistakes to avoid

- Writing R's own vocabulary: there is no `numeric`, `character(1)`, `logical`,
  `integer`, `list<T>`, `Array<T>`, `T[]`, `NULL` (it is `null`), `TRUE`
  (it is `tt`).
- Using `chr` for "a string": `chr1` *is* `vec1`, so `chr` accepts numbers.
  Write `CHR1`.
- Using `DBL` for an ordinary numeric parameter: it excludes `f(1L)`, which
  really works. Use `dbl` unless the exact mode is load-bearing (§3.1).
- Writing `dbl \ int` where `DBL` says the same thing, or any other subtraction
  that has a plain name. Simpler is better.
- Forgetting `?` on parameters that have defaults. Then every ordinary call is a
  type error.
- Forgetting that `^` *removes* `NA` rather than announcing it — and that it is
  only allowed on length-1 types.
- Renaming or reordering parameters: they must match the R formals exactly.
- Writing `<data.frame>` (exactly that one class) where a tibble-like object with
  several classes is meant: use `<data.frame, ...>`.
- Adding attributes/classes constraints where none are needed: a bare `dbl`
  already allows any.
