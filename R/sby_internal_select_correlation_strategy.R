#' @title Internal Helper: Select Correlation Strategy
#'
#' @usage sby_internal_select_correlation_strategy(selected_data)
#'
#' @description
#' Select automatic correlation backend from data size thresholds
#'
#' @details
#' The strategy uses `prod(dim(selected_data))` and configured
#' options to select streaming or Fortran execution.
#'
#' The former `blas` path (R-level sweep + crossprod + outer) was
#' removed: it allocated 4-5 full matrix copies in interpreted R
#' before MKL touched data. The Fortran path calls oneMKL ssyrk
#' directly on REAL*4 in-place memory and is strictly faster.
#'
#' @param selected_data Numeric tabular object selected for correlation
#'
#' @return A single strategy label: `"streaming"` or `"fortran"`
#'
#' @examples
#' sample_data <- matrix(stats::rnorm(100), ncol = 5)
#' sby_internal_select_correlation_strategy(selected_data = sample_data)
sby_internal_select_correlation_strategy <- function(selected_data){

  selected_data_size <- prod(dim(selected_data))
  start_fortran <- getOption("sby_config_start_fortran", 10000L)

  if(selected_data_size >= start_fortran){
    return("fortran")
  }

  return("streaming")
}
####
## End
#
