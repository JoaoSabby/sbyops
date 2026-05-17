#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP sby_modal_frequency_codes_fortran(SEXP codes_list, SEXP max_codes);
extern SEXP sby_correlation_pearson_matrix_fortran(SEXP matrix, SEXP n_rows, SEXP n_cols);

static const R_CallMethodDef CallEntries[] = {
  {"sby_modal_frequency_codes_fortran", (DL_FUNC) &sby_modal_frequency_codes_fortran, 2},
  {"sby_correlation_pearson_matrix_fortran", (DL_FUNC) &sby_correlation_pearson_matrix_fortran, 3},
  {NULL, NULL, 0}
};

void R_init_sbyops(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
