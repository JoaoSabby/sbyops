#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/*
 * Registro das rotinas nativas.
 *
 * A função Fortran foi exposta com:
 *
 *   bind(C, name = "sby_nzv_codes_fortran")
 *
 * Por isso o símbolo C abaixo tem exatamente o mesmo nome, sem sufixo de
 * compilador Fortran.
 */
extern SEXP sby_nzv_codes_fortran(SEXP codes_list, SEXP max_codes, SEXP threshold, SEXP n_threads);

static const R_CallMethodDef CallEntries[] = {
  {"sby_fe_filter_nzv_fortran", (DL_FUNC) &sby_nzv_codes_fortran, 4},
  {NULL, NULL, 0}
};

void R_init_sbyops(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
