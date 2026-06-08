#' @title Internal Helper: Compute Correlation Fortran
#'
#' @usage sby_internal_compute_correlation_fortran(numeric_matrix, threshold)
#'
#' @description Compute correlation and pruning through native Fortran backend
#'
#' @param numeric_matrix A double numeric matrix
#' @param threshold A numeric scalar in `[0, 1]`
#'
#' @return A character vector of removed columns
#'
#' @examples
#' sample_matrix <- matrix(stats::rnorm(20), ncol = 2)
#' colnames(sample_matrix) <- c("A", "B")
#' sby_internal_compute_correlation_fortran(numeric_matrix = sample_matrix, threshold = 0.9)
sby_internal_compute_correlation_fortran <- function(numeric_matrix, threshold){

  # Delegate matrix correlation calculation and pruning to native Fortran backend
  keep_logical <- .Call(
    "sby_correlation_pearson_matrix_fortran",
    numeric_matrix,
    as.integer(fnrow(numeric_matrix)),
    as.integer(fncol(numeric_matrix)),
    as.double(threshold),
    PACKAGE = "sbyops"
  )

  # Map logical mask to character vector of removed columns
  removed_columns <- colnames(numeric_matrix)[!as.logical(keep_logical)]

  return(removed_columns)
}
####
## End
# 
