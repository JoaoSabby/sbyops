#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>& Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

NumericVector sby_internal_table_detect_integer_type(IntegerVector current_column);
NumericVector sby_internal_table_detect_numeric_type(NumericVector current_column);

RcppExport SEXP _sbyops_sby_internal_table_detect_integer_type(SEXP current_column_sexp) {
BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rng_scope_gen;
  Rcpp::traits::input_parameter<IntegerVector>::type current_column(current_column_sexp);
  rcpp_result_gen = Rcpp::wrap(sby_internal_table_detect_integer_type(current_column));
  return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP _sbyops_sby_internal_table_detect_numeric_type(SEXP current_column_sexp) {
BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rng_scope_gen;
  Rcpp::traits::input_parameter<NumericVector>::type current_column(current_column_sexp);
  rcpp_result_gen = Rcpp::wrap(sby_internal_table_detect_numeric_type(current_column));
  return rcpp_result_gen;
END_RCPP
}
