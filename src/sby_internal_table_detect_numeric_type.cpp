//' @title Detect Numeric Vector Metadata for Arrow Schema Optimization
//'
//' @description
//' Performs a single-pass scan over a numeric vector to collect metadata
//' required for efficient Apache Arrow type inference.
//'
//' @details
//' This function is intended for internal use in high-performance Parquet
//' writing routines. In a single pass over the vector, it evaluates whether
//' the column has valid values, whether it contains non-finite values, whether
//' all finite values are mathematical integers, whether all finite values can
//' be represented as numeric booleans, and what the minimum and maximum finite
//' values are.
//'
//' Missing values, including \code{NA} and \code{NaN}, are ignored. Infinite
//' values are detected and prevent safe conversion to integer Arrow types.
//'
//' The function returns an unnamed numeric vector to reduce allocations at the
//' C++ and R interface. The positions must be interpreted as follows:
//' \enumerate{
  //'   \item Presence of at least one non-missing value
//'   \item Presence of at least one non-finite value
  //'   \item Integer nature of all finite values
//'   \item Boolean representation feasibility
  //'   \item Minimum finite value
//'   \item Maximum finite value
  //' }
//'
  //' @param current_column Numeric vector from R.
//'
  //' @return Numeric vector with six positions.
//'
  //' @usage sby_internal_table_detect_numeric_type(current_column)
//'
  //' @keywords internal

#include <Rcpp.h>
#include <cmath>
#include <limits>

using namespace Rcpp;

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
