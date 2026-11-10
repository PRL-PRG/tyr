# The type language

A type is a set of R values; subtyping is set inclusion.

## Algebra

`any` (every value), `empty` (no value), `T | U` (union), `T & U`
(intersection), `T \ U` (difference), `~T` (negation), `(T)` (grouping).

## Atomic vectors

| Type | Values |
|---|---|
| `LGL`, `INT`, `DBL`, `CLX`, `CHR`, `RAW` | a vector of exactly that mode, any length |
| `lgl`, `int`, `dbl`, `clx`, `chr`, `raw` | that mode and every mode R coerces to it: `raw < lgl < int < dbl < clx`, and `chr` above all |
| `vec` | any atomic vector (a list is not one) |
| `DBL1`, `dbl1`, `CHR1`, `vec1`, ... | the same, of length exactly 1 |

- **`dbl` contains logicals and integers**: it is the right type for an
  ordinary numeric parameter, since `f(1L)` and `f(TRUE)` work.
- **`chr` is any atomic vector** (`chr1` is `vec1`): it says nothing. A
  character vector is `CHR`, a string is `CHR1`.
- `DBL` only where the exact storage type matters: a value passed to C code
  (`.Call`), `typeof`-like reflection, or a result callers rely on.
- Literals are scalar types: `42L`, `42.`, `"txt"`, `tt` (TRUE), `ff` (FALSE).
- Every type contains `NA` except literals. `^` removes `NA`, and only from
  scalars: `^DBL1`, `^LGL1` (= `tt | ff`). `^DBL` is an error.

## Other values

`null` (NULL; not a vector: a function that may return NULL needs `| null`),
`env`, `sym` (a symbol), `lang` (a call, a formula), `externalptr`.

## Lists

`{ a: T, b: U }` (exactly the fields `a` and `b`), `{ a: T, U }` (field `a`,
every other element of type `U`), `{ T }` (any number of `T`), `{ any }` (any
list), `{ a: T? }` (`a` may be absent).

## Classes and attributes

A bare type allows any class and attributes. To restrict: `<cls>` (exactly
class `cls`), `<cls, ...>` (at least `cls`), `<>` (no class), and
`T with { dim: INT }` for other attributes. An S3 object of class `foo`:
`<foo, ...>`, or `dbl & <difftime, ...>`.

## Functions

`(x: T, y: U?, ...: V) -> W`:
- the parameters are the R function's formals, **same names, same order**;
- `?` marks an optional parameter: every parameter with a default value, or
  only used under `if (!missing(p))`, needs it;
- `...: V` types the extra arguments (`...: any` if anything goes);
- `->`, not `-->`.

A parameter that is itself a function is typed by how it is called:
`f: @(dbl) -> dbl` (called with one `dbl`), never `f: (x: dbl) -> dbl`.

Several behaviours are an intersection of arrows, each in parentheses:
`((x: dbl1) -> CHR1) & ((x: dbl) -> CHR)` (a scalar for a scalar, a vector
for a vector).

Type variables tie a result to an argument: `(x: 'a) -> 'a`,
`(x: v('a)) -> v1('a)` (a vector of some mode to a scalar of the same mode),
`(X: { 'a }) -> { 'a }`.

## Not the type language

There is no `numeric`, `character`, `logical`, `integer`, `list<T>`, `T[]`,
`NULL` (it is `null`) or `TRUE` (it is `tt`).

## Examples

```
(x: CHR1) -> null
(bytes: DBL | INT, style: CHR?) -> CHR
(x: dbl, minval: dbl1?) -> CHR
(x: dbl, digits: dbl?, sci_range: dbl1?, sci_sep: CHR1?) -> CHR
(date: <POSIXct, POSIXt, ...>, format: CHR?) -> CHR
(bytes: DBL | INT, smallest_unit: CHR1?) -> { amount: DBL, unit: CHR, negative: LGL }<data.frame>
```
