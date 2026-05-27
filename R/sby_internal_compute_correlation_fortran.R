#' @title Internal Helper: Compute Correlation Fortran
#'
#' @usage sby_internal_compute_correlation_fortran(numeric_matrix)
#'
#' @description Compute absolute Pearson correlation matrix through native Fortran backend
#'
#' @param numeric_matrix A double numeric matrix
#'
#' @return A square numeric correlation matrix
#'
#' @examples
#' sample_matrix <- matrix(stats::rnorm(20), ncol = 2)
#' sby_internal_compute_correlation_fortran(numeric_matrix = sample_matrix)
sby_internal_compute_correlation_fortran <- function(numeric_matrix){

  # Delegate matrix correlation calculation to native Fortran backend
  native_vector <- .Call(
    "sby_correlation_pearson_matrix_fortran",
    numeric_matrix,
    as.integer(fnrow(numeric_matrix)),
    as.integer(fncol(numeric_matrix)),
    PACKAGE = "sbyops"
  )

  # Build square matrix from native payload values
  correlation_matrix <- matrix(
    data = native_vector,
    nrow = ncol(numeric_matrix),
    ncol = ncol(numeric_matrix)
  )

  # Return reshaped correlation matrix from Fortran backend
  return(correlation_matrix)
}
####
## End
# 
