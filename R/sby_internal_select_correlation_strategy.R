#' @title Internal Helper: Select Correlation Strategy
#'
#' @usage sby_internal_select_correlation_strategy(selected_data)
#'
#' @description
#' Select automatic correlation backend from data size thresholds
#'
#' @details
#' The strategy uses `prod(dim(selected_data))` and configured
#' options to select streaming, Fortran, or BLAS execution
#'
#' @param selected_data Numeric tabular object selected for correlation
#'
#' @return A single strategy label: `"streaming"`, `"fortran"`, or
#' `"blas"`
#'
#' @examples
#' sample_data <- matrix(stats::rnorm(100), ncol = 5)
#' sby_internal_select_correlation_strategy(selected_data = sample_data)
sby_internal_select_correlation_strategy <- function(selected_data){

  # Compute workload size from selected data dimensions
  selected_data_size <- prod(dim(selected_data))

  # Load configured backend thresholds
  start_fortran <- getOption("sby_config_start_fortran", 10000L)
  start_blas <- getOption("sby_config_start_blas", 100000L)

  # Select BLAS backend for largest workloads
  if(selected_data_size >= start_blas){
    selected_strategy <- "blas"

    # Return selected backend strategy for largest workloads
    return(selected_strategy)
  }

  # Select Fortran backend for medium workloads
  if(selected_data_size > start_fortran){
    selected_strategy <- "fortran"

    # Return selected backend strategy for medium workloads
    return(selected_strategy)
  }

  # Select streaming backend for smaller workloads
  selected_strategy <- "streaming"

  # Return selected backend strategy for smaller workloads
  return(selected_strategy)
}
####
## End
# 
