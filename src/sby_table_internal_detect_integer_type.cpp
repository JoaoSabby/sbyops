// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <limits>

using namespace Rcpp;

//' @title Detect Integer Vector Metadata for Arrow Schema Optimization
//'
//' @description
//' Performs a single-pass scan over an integer vector to collect metadata
//' required for efficient Apache Arrow type inference.
//'
//' @details
//' This function avoids separate calls for minimum, maximum, and boolean
//' feasibility checks on integer columns. In a single pass over the vector, it
//' evaluates whether the column has valid values, whether all valid values can
//' be represented as booleans, and what the minimum and maximum values are.
//'
//' Missing values are ignored. Since the input vector is already an integer
//' vector in R, no fractional component check is required.
//'
//' The function returns an unnamed numeric vector to reduce allocations at the
//' C++ and R interface. The positions must be interpreted as follows:
//' \enumerate{
//'   \item Presence of at least one non-missing value
//'   \item Boolean representation feasibility
//'   \item Minimum value
//'   \item Maximum value
//' }
//'
//' @param current_column Integer vector from R.
//'
//' @return Numeric vector with four positions.
//'
//' @usage sby_table_internal_detect_integer_type(current_column)
//'
//' @keywords internal

// [[Rcpp::export]]
NumericVector sby_table_internal_detect_integer_type(IntegerVector current_column) {
  
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
####
## Fim
#
