#' @title Internal Helper: Compute Correlation Streaming
#'
#' @usage sby_internal_compute_correlation_streaming(mat)
#'
#' @description
#' Compute absolute correlation using a memory-aware centered-matrix
#' pipeline for package-internal selection workflows
#'
#' @details
#' This helper performs column centering once, computes covariance
#' with linear algebra kernels, and then normalizes values into
#' absolute Pearson correlation coefficients
#'
#' @section Internal contract:
#' This function expects a numeric matrix already validated by
#' upstream internal guards
#'
#' @param mat A numeric matrix used for correlation estimation
#'
#' @return A square absolute-correlation matrix with sanitized values
#'
#' @seealso [sby_select_correlation()]
#'
#' @references
#' R CORE TEAM. Writing R Extensions. Vienna: R Foundation.
#' Available at:
#' https://cran.r-project.org/doc/manuals/r-release/R-exts.html
#'
#' @examples
#' example_matrix <- matrix(rnorm(20), ncol = 4)
#' sby_internal_compute_correlation_streaming(mat = example_matrix)
sby_internal_compute_correlation_streaming <- function(mat){

  # Compute per-column means used for centered covariance estimation
  column_means <- colMeans(mat)

  # Center matrix values to remove location effects before crossprod
  centered_matrix <- sweep(mat, 2L, column_means, FUN = "-")

  # Compute per-column sums of squares for normalization terms
  sum_squares <- colSums(centered_matrix * centered_matrix)

  # Compute covariance matrix through optimized matrix multiplication
  covariance_matrix <- crossprod(centered_matrix)

  # Build denominator matrix for Pearson normalization
  denominator_matrix <- sqrt(outer(sum_squares, sum_squares))

  # Convert covariance values to absolute correlation coefficients
  correlation_matrix <- abs(covariance_matrix / denominator_matrix)

  # Replace non-finite results introduced by zero-variance columns
  correlation_matrix[!is.finite(correlation_matrix)] <- 0

  # Clear diagonal to simplify downstream threshold comparisons
  diag(correlation_matrix) <- 0

  # Return sanitized absolute-correlation matrix for selection
  return(correlation_matrix)
}
####
## End
# 
