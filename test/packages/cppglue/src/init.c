#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* Declared at SEXP types to be registered: the glue gives the types. */
SEXP _cppglue_scale_by(SEXP, SEXP);
SEXP _cppglue_all_named(SEXP);

static const R_CallMethodDef callMethods[] = {
    {"_cppglue_scale_by",  (DL_FUNC) &_cppglue_scale_by,  2},
    {"_cppglue_all_named", (DL_FUNC) &_cppglue_all_named, 1},
    {NULL, NULL, 0}};

void R_init_cppglue(DllInfo *info)
{
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
}
