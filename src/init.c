
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP sby_internal_correlation_pearson_matrix_fortran(SEXP matrix, SEXP n_rows, SEXP n_cols, SEXP threshold);
extern SEXP sby_internal_non_constant_mask(SEXP cols);
extern SEXP _sbyops_sby_internal_table_detect_integer_type(SEXP current_column);
extern SEXP _sbyops_sby_internal_table_detect_numeric_type(SEXP current_column);

// @title Tabela de rotinas nativas expostas ao R
// @description Centraliza o registro para chamadas .Call com validacao de
// assinatura e impede dependencias de simbolos dinamicos nao registrados.
// @details A tabela associa nomes usados por R a ponteiros C ou C++ e ao numero
// de argumentos esperados. Nao aloca memoria e e consumida por R_init_sbyops.
static const R_CallMethodDef CallEntries[] = {
  {"sby_internal_correlation_pearson_matrix_fortran", (DL_FUNC) &sby_internal_correlation_pearson_matrix_fortran, 4},
  {"sby_internal_non_constant_mask", (DL_FUNC) &sby_internal_non_constant_mask, 1},
  {"_sbyops_sby_internal_table_detect_integer_type", (DL_FUNC) &_sbyops_sby_internal_table_detect_integer_type, 1},
  {"_sbyops_sby_internal_table_detect_numeric_type", (DL_FUNC) &_sbyops_sby_internal_table_detect_numeric_type, 1},
  {NULL, NULL, 0}
};

// @title Inicializar a DLL do pacote
// @description Registra rotinas nativas e bloqueia simbolos dinamicos nao
// registrados no carregamento do pacote pelo R.
// @details A funcao e chamada pelo mecanismo de inicializacao nativa do R. Ela
// nao retorna valor ao R e modifica apenas a tabela de registro da DLL.
// @param dll Ponteiro para metadados da DLL carregada pelo R.
// @return Sem retorno.
void R_init_sbyops(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
