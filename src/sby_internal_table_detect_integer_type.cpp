// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <limits>

using namespace Rcpp;

// @title Detectar metadados de vetor inteiro para esquema Arrow
// @description Examina uma coluna recebida do R em uma unica passagem para
// apoiar a inferencia interna de tipos Arrow na escrita Parquet.
// @details A rotina C++ nao assume ownership da memoria do vetor R, nao altera
// a entrada e retorna um vetor compacto sem nomes. Valores ausentes sao tratados
// conforme a implementacao real da funcao. O uso direto e interno e deve
// permanecer alinhado ao wrapper R gerado por Rcpp.
// @param current_column Vetor recebido do R pela interface Rcpp.
// @return vetor numerico de quatro posicoes com indicadores de metadados.
// @seealso sby_table_optimize_scheme

// [[Rcpp::export]]
NumericVector sby_internal_table_detect_integer_type(IntegerVector current_column) {
  
  // Use R_xlen_t to support long R vectors
  const R_xlen_t vector_size = current_column.size();
  
  // Keep scalar states to reduce allocations
  bool has_value = false;
  bool is_boolean = true;
  
  // Use integer sentinels to update limits in a single pass
  int min_value = std::numeric_limits<int>::max();
  int max_value = std::numeric_limits<int>::min();
  
  // Scan the column only once
  for(R_xlen_t i = 0; i < vector_size; ++i) {
    
    // Copy to a local scalar to reduce repeated vector access
    const int current_value = current_column[i];
    
    // Ignore integer NA during type inference
    if(current_value == NA_INTEGER) {
      continue;
    }
    
    // Register that at least one analyzable value exists
    has_value = true;
    
    // Update minimum and maximum values without auxiliary structures
    if(current_value < min_value) {
      min_value = current_value;
    }
    
    if(current_value > max_value) {
      max_value = current_value;
    }
    
    // Test boolean feasibility only while the column remains a candidate
    if(is_boolean && current_value != 0 && current_value != 1) {
      is_boolean = false;
    }
  }
  
  // Keep deterministic placeholders when the column has only missing values
  if(!has_value) {
    min_value = 0;
    max_value = 0;
  }

  // Return a compact unnamed vector to reduce allocations
  return NumericVector::create(
    has_value ? 1.0 : 0.0,
    is_boolean ? 1.0 : 0.0,
    static_cast<double>(min_value),
    static_cast<double>(max_value)
  );
}
