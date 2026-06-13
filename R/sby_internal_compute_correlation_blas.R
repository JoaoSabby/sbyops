#' @title Internal Helper: Compute Correlation BLAS (DEPRECATED)
#'
#' @description
#' Deprecated. Previously used R-level sweep + crossprod + outer for
#' Pearson correlation. Replaced by the Fortran path which calls
#' oneMKL ssyrk directly on REAL*4 in-place memory.
#'
#' This file is kept to avoid breaking any direct calls from external
#' code. It will be removed in a future version.
#'
#' @param mat A numeric matrix
#' @return A square absolute-correlation matrix
#' @keywords internal
sby_internal_compute_correlation_blas <- function(mat){
  .Deprecated("sby_internal_compute_correlation_fortran")

  column_means    <- colMeans(mat)
  centered_matrix <- sweep(mat, 2L, column_means, FUN = "-")
  sum_squares     <- colSums(centered_matrix * centered_matrix)
  covariance_matrix  <- crossprod(centered_matrix)
  denominator_matrix <- sqrt(outer(sum_squares, sum_squares))
  correlation_matrix <- abs(covariance_matrix / denominator_matrix)
  correlation_matrix[!is.finite(correlation_matrix)] <- 0
  diag(correlation_matrix) <- 0
  correlation_matrix
}
####
## End
#
