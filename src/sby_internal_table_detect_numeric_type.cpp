#include <Rcpp.h>
#include <cmath>
#include <limits>

using namespace Rcpp;

// @title Detectar metadados de vetor numerico para esquema Arrow
// @description Examina uma coluna recebida do R em uma unica passagem para
// apoiar a inferencia interna de tipos Arrow na escrita Parquet.
// @details A rotina C++ nao assume ownership da memoria do vetor R, nao altera
// a entrada e retorna um vetor compacto sem nomes. Valores ausentes sao tratados
// conforme a implementacao real da funcao. O uso direto e interno e deve
// permanecer alinhado ao wrapper R gerado por Rcpp.
// @param current_column Vetor recebido do R pela interface Rcpp.
// @return vetor numerico de seis posicoes com indicadores de metadados.
// @seealso sby_table_optimize_scheme

// [[Rcpp::export]]
NumericVector sby_internal_table_detect_numeric_type(NumericVector current_column) {

  // Use R_xlen_t to support long R vectors
  const R_xlen_t vector_size = current_column.size();

  // Keep scalar states to reduce allocations
  bool has_value = false;
  bool has_non_finite = false;
  bool is_integer = true;
  bool is_boolean = true;

  // Use numeric sentinels to update limits in a single pass
  double min_value = std::numeric_limits<double>::infinity();
  double max_value = -std::numeric_limits<double>::infinity();

  // Scan the column only once
  for(R_xlen_t i = 0; i < vector_size; ++i) {

    // Copy to a local scalar to reduce repeated vector access
    const double current_value = current_column[i];

    // Ignore NA and NaN during type inference
    if(R_IsNA(current_value) || R_IsNaN(current_value)) {
      continue;
    }

    // Register that at least one analyzable value exists
    has_value = true;

    // Non-finite values must not be converted to integer types
    if(!std::isfinite(current_value)) {
      has_non_finite = true;
      is_integer = false;
      is_boolean = false;
      continue;
    }

    // Update minimum and maximum values without auxiliary structures
    if(current_value < min_value) {
      min_value = current_value;
    }

    if(current_value > max_value) {
      max_value = current_value;
    }

    // Test boolean feasibility only while the column remains a candidate
    if(is_boolean && current_value != 0.0 && current_value != 1.0) {
      is_boolean = false;
    }

    // Test integer feasibility only while the column remains a candidate
    if(is_integer && std::trunc(current_value) != current_value) {
      is_integer = false;
    }
  }

  // Return a compact unnamed vector to reduce allocations
  return NumericVector::create(
    has_value ? 1.0 : 0.0,
    has_non_finite ? 1.0 : 0.0,
    is_integer ? 1.0 : 0.0,
    is_boolean ? 1.0 : 0.0,
    min_value,
    max_value
  );
}
