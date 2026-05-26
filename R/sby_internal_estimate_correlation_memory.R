#' @title Internal Helper: Estimate Correlation Memory
#'
#' @usage sby_internal_estimate_correlation_memory(n_rows, n_cols)
#'
#' @description
#' Estimate memory footprint for numeric matrix materialization and
#' square correlation matrix allocation
#'
#' @details
#' Estimates assume 8 bytes per double-precision value and produce a
#' compact payload consumed by strategy-selection routines
#'
#' @param n_rows Number of matrix rows
#'
#' @param n_cols Number of matrix columns
#'
#' @return A named list with `matrix_bytes` and `corr_bytes`
#'
#' @seealso [sby_internal_select_correlation_strategy()]
#'
#' @references
#' R CORE TEAM. Writing R Extensions. Vienna: R Foundation.
#' \href{https://cran.r-project.org/doc/manuals/r-release/R-exts.html}{R Extensions Manual}
#'
#' @examples
#' sby_internal_estimate_correlation_memory(n_rows = 1000L, n_cols = 50L)
sby_internal_estimate_correlation_memory <- function(n_rows, n_cols){

  # Compute bytes required to materialize the centered matrix
  matrix_bytes <- as.numeric(n_rows) * as.numeric(n_cols) * 8

  # Compute bytes required for the square correlation matrix
  corr_bytes <- as.numeric(n_cols) * as.numeric(n_cols) * 8

  # Build memory-profile payload for backend strategy dispatch
  memory_profile <- list(matrix_bytes = matrix_bytes, corr_bytes = corr_bytes)

  # Return memory-profile payload for downstream decision logic
  return(memory_profile)
}
####
## End
# 
