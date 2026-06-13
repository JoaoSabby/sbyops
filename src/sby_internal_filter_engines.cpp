#include <Rcpp.h>
#include <mkl.h>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>
#include <string>
#include <unordered_map>
#include <vector>
#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

namespace {

std::string make_column_key(SEXP column, R_xlen_t row_index) {
  switch(TYPEOF(column)) {
  case INTSXP: {
    IntegerVector values(column);
    int current_value = values[row_index];
    if(current_value == NA_INTEGER) {
      return "<NA_INTEGER>";
    }
    return std::string("i:") + std::to_string(current_value);
  }
  case REALSXP: {
    NumericVector values(column);
    double current_value = values[row_index];
    if(R_IsNA(current_value) || R_IsNaN(current_value)) {
      return "<NA_REAL>";
    }
    return std::string("d:") + std::to_string(current_value);
  }
  default: {
    stop("sbyops expects only integer or double columns.");
    return "";
  }
  }
}

double get_numeric_value(SEXP numeric_data, int row_index, int column_index, int n_rows) {
  if(Rf_isMatrix(numeric_data)) {
    switch(TYPEOF(numeric_data)) {
    case REALSXP: {
      NumericVector values(numeric_data);
      return values[static_cast<R_xlen_t>(column_index) * n_rows + row_index];
    }
    case INTSXP: {
      IntegerVector values(numeric_data);
      const int current_value = values[static_cast<R_xlen_t>(column_index) * n_rows + row_index];
      return current_value == NA_INTEGER ? NA_REAL : static_cast<double>(current_value);
    }
    default:
      return NA_REAL;
    }
  }

  List numeric_columns(numeric_data);
  SEXP current_column = numeric_columns[column_index];
  switch(TYPEOF(current_column)) {
  case REALSXP: {
    NumericVector values(current_column);
    return values[row_index];
  }
  case INTSXP: {
    IntegerVector values(current_column);
    const int current_value = values[row_index];
    return current_value == NA_INTEGER ? NA_REAL : static_cast<double>(current_value);
  }
  default:
    return NA_REAL;
  }
}

int get_native_row_count(SEXP numeric_data) {
  if(Rf_isMatrix(numeric_data)) {
    IntegerVector dimensions = Rf_getAttrib(numeric_data, R_DimSymbol);
    return dimensions[0];
  }

  List numeric_columns(numeric_data);
  return numeric_columns.size() == 0 ? 0 : static_cast<int>(Rf_xlength(numeric_columns[0]));
}

int get_native_column_count(SEXP numeric_data) {
  if(Rf_isMatrix(numeric_data)) {
    IntegerVector dimensions = Rf_getAttrib(numeric_data, R_DimSymbol);
    return dimensions[1];
  }

  List numeric_columns(numeric_data);
  return numeric_columns.size();
}

CharacterVector get_native_column_names(SEXP numeric_data) {
  if(Rf_isMatrix(numeric_data)) {
    SEXP dimension_names_sexp = Rf_getAttrib(numeric_data, R_DimNamesSymbol);
    if(!Rf_isNull(dimension_names_sexp)) {
      List dimension_names(dimension_names_sexp);
      if(dimension_names.size() == 2 && !Rf_isNull(dimension_names[1])) {
        return as<CharacterVector>(dimension_names[1]);
      }
    }
  }

  List numeric_columns(numeric_data);
  return numeric_columns.names();
}

std::vector<float> build_z_score_buffer(SEXP numeric_data, int n_rows, int n_cols) {
  std::vector<float> z_score_buffer(static_cast<std::size_t>(n_rows) * n_cols, std::numeric_limits<float>::quiet_NaN());

  for(int column_index = 0; column_index < n_cols; ++column_index) {
    double sum_value = 0.0;
    double sum_square = 0.0;
    int finite_count = 0;

    for(int row_index = 0; row_index < n_rows; ++row_index) {
      const double current_value = get_numeric_value(numeric_data, row_index, column_index, n_rows);
      if(std::isfinite(current_value)) {
        sum_value += current_value;
        sum_square += current_value * current_value;
        ++finite_count;
      }
    }

    if(finite_count < 2) {
      continue;
    }

    const double mean_value = sum_value / static_cast<double>(finite_count);
    const double variance_value = (sum_square - (sum_value * sum_value / static_cast<double>(finite_count))) /
      static_cast<double>(finite_count - 1);
    const double standard_deviation = std::sqrt(std::max(variance_value, 0.0));

    if(!(standard_deviation > 0.0) || !std::isfinite(standard_deviation)) {
      continue;
    }

    for(int row_index = 0; row_index < n_rows; ++row_index) {
      const double current_value = get_numeric_value(numeric_data, row_index, column_index, n_rows);
      if(std::isfinite(current_value)) {
        z_score_buffer[static_cast<std::size_t>(column_index) * n_rows + row_index] =
          static_cast<float>((current_value - mean_value) / standard_deviation);
      }
    }
  }

  return z_score_buffer;
}

float compute_pair_correlation(const std::vector<float>& z_score_buffer, int n_rows, int first_column, int second_column) {
  const float* first_values = z_score_buffer.data() + static_cast<std::size_t>(first_column) * n_rows;
  const float* second_values = z_score_buffer.data() + static_cast<std::size_t>(second_column) * n_rows;
  float dot_product = 0.0f;
  int complete_count = 0;

  for(int row_index = 0; row_index < n_rows; ++row_index) {
    const float first_value = first_values[row_index];
    const float second_value = second_values[row_index];
    if(std::isfinite(first_value) && std::isfinite(second_value)) {
      dot_product += first_value * second_value;
      ++complete_count;
    }
  }

  if(complete_count < 2) {
    return 0.0f;
  }

  const float scale_value = static_cast<float>(complete_count - 1);
  const float correlation_value = std::fabs(dot_product / scale_value);
  return std::isfinite(correlation_value) ? std::min(correlation_value, 1.0f) : 0.0f;
}

bool has_complete_z_score_buffer(const std::vector<float>& z_score_buffer) {
  for(std::size_t value_index = 0; value_index < z_score_buffer.size(); ++value_index) {
    if(!std::isfinite(z_score_buffer[value_index])) {
      return false;
    }
  }

  return true;
}

void configure_onemkl_runtime() {
  mkl_set_dynamic(0);
  mkl_enable_instructions(MKL_ENABLE_AVX512);
#ifdef _OPENMP
  mkl_set_num_threads_local(omp_get_max_threads());
#endif
}

std::vector<float> compute_correlation_values_blas(const std::vector<float>& z_score_buffer, int n_rows, int n_cols) {
  std::vector<float> correlation_values(static_cast<std::size_t>(n_cols) * n_cols, 0.0f);
  const int result_rows = n_cols;
  const int result_cols = n_cols;
  const int shared_dim = n_rows;
  const int leading_input = n_rows;
  const int leading_result = n_cols;
  const float alpha_value = n_rows > 1 ? 1.0f / static_cast<float>(n_rows - 1) : 0.0f;
  const float beta_value = 0.0f;

  configure_onemkl_runtime();
  cblas_sgemm(
    CblasColMajor,
    CblasTrans,
    CblasNoTrans,
    result_rows,
    result_cols,
    shared_dim,
    alpha_value,
    z_score_buffer.data(),
    leading_input,
    z_score_buffer.data(),
    leading_input,
    beta_value,
    correlation_values.data(),
    leading_result
  );

  for(std::size_t value_index = 0; value_index < correlation_values.size(); ++value_index) {
    const float correlation_value = std::fabs(correlation_values[value_index]);
    correlation_values[value_index] = std::isfinite(correlation_value) ? std::min(correlation_value, 1.0f) : 0.0f;
  }

  for(int column_index = 0; column_index < n_cols; ++column_index) {
    correlation_values[static_cast<std::size_t>(column_index) * n_cols + column_index] = 0.0f;
  }

  return correlation_values;
}

std::vector<float> compute_correlation_values_pairwise(const std::vector<float>& z_score_buffer, int n_rows, int n_cols) {
  std::vector<float> correlation_values(static_cast<std::size_t>(n_cols) * n_cols, 0.0f);

  #pragma omp parallel for schedule(dynamic) if(n_cols > 8)
  for(int first_column = 0; first_column < n_cols; ++first_column) {
    for(int second_column = first_column + 1; second_column < n_cols; ++second_column) {
      const float correlation_value = compute_pair_correlation(z_score_buffer, n_rows, first_column, second_column);
      correlation_values[static_cast<std::size_t>(first_column) * n_cols + second_column] = correlation_value;
      correlation_values[static_cast<std::size_t>(second_column) * n_cols + first_column] = correlation_value;
    }
  }

  return correlation_values;
}

} // namespace

extern "C" SEXP sby_internal_correlation_removed_columns_cpp(SEXP numeric_data_sexp, SEXP threshold_sexp) {
BEGIN_RCPP
  const double threshold_value = as<double>(threshold_sexp);
  const int n_rows = get_native_row_count(numeric_data_sexp);
  const int n_cols = get_native_column_count(numeric_data_sexp);
  CharacterVector column_names = get_native_column_names(numeric_data_sexp);
  std::vector<float> z_score_buffer = build_z_score_buffer(numeric_data_sexp, n_rows, n_cols);
  std::vector<float> correlation_values = has_complete_z_score_buffer(z_score_buffer) ?
    compute_correlation_values_blas(z_score_buffer, n_rows, n_cols) :
    compute_correlation_values_pairwise(z_score_buffer, n_rows, n_cols);

  std::vector<unsigned char> active_columns(n_cols, 1U);
  std::vector<std::string> removed_columns;

  while(true) {
    float max_correlation = -1.0f;
    int first_selected = -1;
    int second_selected = -1;

    for(int first_column = 0; first_column < n_cols; ++first_column) {
      if(!active_columns[first_column]) continue;
      for(int second_column = first_column + 1; second_column < n_cols; ++second_column) {
        if(!active_columns[second_column]) continue;
        const float correlation_value = correlation_values[static_cast<std::size_t>(first_column) * n_cols + second_column];
        if(correlation_value > max_correlation) {
          max_correlation = correlation_value;
          first_selected = first_column;
          second_selected = second_column;
        }
      }
    }

    if(first_selected < 0 || static_cast<double>(max_correlation) < threshold_value) {
      break;
    }

    double first_mean = 0.0;
    double second_mean = 0.0;
    int active_count = 0;
    for(int column_index = 0; column_index < n_cols; ++column_index) {
      if(!active_columns[column_index]) continue;
      ++active_count;
      if(column_index != first_selected) first_mean += correlation_values[static_cast<std::size_t>(first_selected) * n_cols + column_index];
      if(column_index != second_selected) second_mean += correlation_values[static_cast<std::size_t>(second_selected) * n_cols + column_index];
    }
    if(active_count > 0) {
      first_mean /= static_cast<double>(active_count);
      second_mean /= static_cast<double>(active_count);
    }

    const int removal_index = (first_mean > second_mean) ? first_selected : second_selected;
    active_columns[removal_index] = 0U;
    removed_columns.push_back(as<std::string>(column_names[removal_index]));
  }

  return wrap(removed_columns);
END_RCPP
}

extern "C" SEXP sby_internal_modal_frequency_removed_columns_cpp(SEXP selected_data_sexp, SEXP threshold_sexp) {
BEGIN_RCPP
  List selected_data(selected_data_sexp);
  CharacterVector column_names = selected_data.names();
  const double threshold_value = as<double>(threshold_sexp);
  const int n_cols = selected_data.size();
  const R_xlen_t n_rows = n_cols == 0 ? 0 : Rf_xlength(selected_data[0]);
  const double cutoff_value = std::ceil(threshold_value * static_cast<double>(n_rows));
  std::vector<std::string> removed_columns;

  for(int column_index = 0; column_index < n_cols; ++column_index) {
    SEXP column = selected_data[column_index];
    std::unordered_map<std::string, R_xlen_t> frequency_map;
    R_xlen_t modal_count = 0;

    for(R_xlen_t row_index = 0; row_index < n_rows; ++row_index) {
      const std::string column_key = make_column_key(column, row_index);
      R_xlen_t current_count = ++frequency_map[column_key];
      if(current_count > modal_count) {
        modal_count = current_count;
      }
    }

    if(static_cast<double>(modal_count) >= cutoff_value) {
      removed_columns.push_back(as<std::string>(column_names[column_index]));
    }
  }

  return wrap(removed_columns);
END_RCPP
}
