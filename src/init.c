
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP sby_internal_correlation_pearson_matrix_fortran(SEXP matrix, SEXP n_rows, SEXP n_cols, SEXP threshold);
extern SEXP sby_internal_non_constant_mask(SEXP cols);
extern SEXP _sbyops_sby_internal_table_detect_integer_type(SEXP current_column);
extern SEXP _sbyops_sby_internal_table_detect_numeric_type(SEXP current_column);
extern SEXP sby_internal_correlation_removed_columns_cpp(SEXP numeric_matrix, SEXP threshold);
extern SEXP sby_internal_modal_frequency_removed_columns_cpp(SEXP selected_data, SEXP threshold);

/**
 * @title Tabela de rotinas nativas expostas ao R
 * @description Centraliza registro para chamadas .Call com validacao de assinatura
 */
static const R_CallMethodDef CallEntries[] = {
  {"sby_internal_correlation_pearson_matrix_fortran", (DL_FUNC) &sby_internal_correlation_pearson_matrix_fortran, 4},
  {"sby_internal_non_constant_mask", (DL_FUNC) &sby_internal_non_constant_mask, 1},
  {"sby_internal_correlation_removed_columns_cpp", (DL_FUNC) &sby_internal_correlation_removed_columns_cpp, 2},
  {"sby_internal_modal_frequency_removed_columns_cpp", (DL_FUNC) &sby_internal_modal_frequency_removed_columns_cpp, 2},
  {"_sbyops_sby_internal_table_detect_integer_type", (DL_FUNC) &_sbyops_sby_internal_table_detect_integer_type, 1},
  {"_sbyops_sby_internal_table_detect_numeric_type", (DL_FUNC) &_sbyops_sby_internal_table_detect_numeric_type, 1},
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
