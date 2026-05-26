
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP sby_modal_frequency_codes_fortran(SEXP codes_list, SEXP max_codes);
extern SEXP sby_correlation_pearson_matrix_fortran(SEXP matrix, SEXP n_rows, SEXP n_cols);
extern SEXP sby_non_constant_mask(SEXP cols);
extern SEXP sby_modal_frequency_mask(SEXP selected_list, SEXP threshold, SEXP max_threads);

/**
 * @title Tabela de rotinas nativas expostas ao R
 * @description Centraliza registro para chamadas .Call com validacao de assinatura
 */
static const R_CallMethodDef CallEntries[] = {
  {"sby_modal_frequency_codes_fortran", (DL_FUNC) &sby_modal_frequency_codes_fortran, 2},
  {"sby_correlation_pearson_matrix_fortran", (DL_FUNC) &sby_correlation_pearson_matrix_fortran, 3},
  {"sby_non_constant_mask", (DL_FUNC) &sby_non_constant_mask, 1},
  {"sby_modal_frequency_mask", (DL_FUNC) &sby_modal_frequency_mask, 3},
  {NULL, NULL, 0}
};

/**
 * @title Inicializa a DLL do pacote
 * @description Registra simbolos nativos e bloqueia simbolos dinamicos nao registrados
 * @param dll Ponteiro para metadados da DLL carregada pelo R
 */
void R_init_sbyops(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
