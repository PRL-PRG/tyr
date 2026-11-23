#include <Rinternals.h>

/* A new double vector: x * k. */
SEXP scale_by(SEXP x, SEXP k) {
  R_xlen_t n = XLENGTH(x);
  SEXP out = PROTECT(Rf_allocVector(REALSXP, n));
  double *px = REAL(x), *po = REAL(out);
  double kk = Rf_asReal(k);
  for (R_xlen_t i = 0; i < n; i++) po[i] = px[i] * kk;
  UNPROTECT(1);
  return out;
}

/* A copy of x with a "unit" attribute and the class "measure". */
SEXP with_unit(SEXP x, SEXP unit) {
  SEXP out = PROTECT(Rf_duplicate(x));
  Rf_setAttrib(out, Rf_install("unit"), unit);
  Rf_setAttrib(out, R_ClassSymbol, Rf_mkString("measure"));
  UNPROTECT(1);
  return out;
}

/* A C callee of count_na. */
static int is_missing(double v) {
  return ISNAN(v);
}

SEXP count_na(SEXP x) {
  R_xlen_t n = XLENGTH(x);
  double *px = REAL(x);
  int count = 0;
  for (R_xlen_t i = 0; i < n; i++) count += is_missing(px[i]);
  return Rf_ScalarInteger(count);
}
