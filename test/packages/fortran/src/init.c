#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void F77_SUB(count)(double *x, int *n, int *k);
void F77_SUB(dist)(double *x, int *n);

SEXP vec_len(SEXP x) { return Rf_ScalarInteger(Rf_length(x)); }

static R_NativePrimitiveArgType scale_t[] = {REALSXP, INTSXP, REALSXP, LGLSXP};

static const R_FortranMethodDef fortranMethods[] = {
    {"scale", (DL_FUNC) &F77_SUB(scale), 4, scale_t},
    {"count", (DL_FUNC) &F77_SUB(count), 3},
    {NULL, NULL, 0}};

/* Registered under another name than the C function's. */
static const R_CallMethodDef callMethods[] = {
    {"pkg_len", (DL_FUNC) &vec_len, 1},
    {NULL, NULL, 0}};

void R_init_fortran(DllInfo *info)
{
    R_registerRoutines(info, NULL, callMethods, fortranMethods, NULL);
    R_useDynamicSymbols(info, TRUE);
}
