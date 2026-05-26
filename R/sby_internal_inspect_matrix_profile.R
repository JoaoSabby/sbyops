#' @title Internal Helper: Inspect Matrix Profile
#'
#' @usage sby_internal_inspect_matrix_profile(mat)
#'
#' @description
#' Build a compact matrix-profile payload used by correlation backend
#' strategy selection
#'
#' @details
#' The profile centralizes row and column dimensions, pair counts,
#' finite-value diagnostics, and memory estimates into a single object
#'
#' @param mat A numeric matrix prepared for correlation processing
#'
#' @return A named list with dimensions, pair count, finite-value flag,
#' and memory estimates
#'
#' @seealso [sby_internal_estimate_correlation_memory()],
#' [sby_internal_select_correlation_strategy()]
#'
#' @references
#' R CORE TEAM. Writing R Extensions. Vienna: R Foundation.
#' \href{https://cran.r-project.org/doc/manuals/r-release/R-exts.html}{R Extensions Manual}
#'
#' @examples
#' example_matrix <- matrix(rnorm(20), ncol = 4)
#' sby_internal_inspect_matrix_profile(mat = example_matrix)
sby_internal_inspect_matrix_profile <- function(mat){

  # Capture matrix row and column dimensions for profile payload
  n_rows <- nrow(mat)
  n_cols <- ncol(mat)

  # Estimate memory footprint used by strategy dispatch
  memory_profile <- sby_internal_estimate_correlation_memory(
    n_rows = n_rows,
    n_cols = n_cols
  )

  # Flag non-finite values that constrain backend selection
  has_non_finite <- any(!is.finite(mat))

  # Compute unique pair count used in complexity heuristics
  n_pairs <- (n_cols * (n_cols - 1L)) / 2L

  # Build profile payload for correlation strategy selection
  matrix_profile <- list(
    n_rows = n_rows,
    n_cols = n_cols,
    n_pairs = n_pairs,
    has_non_finite = has_non_finite,
    matrix_bytes = memory_profile$matrix_bytes,
    corr_bytes = memory_profile$corr_bytes
  )

  # Return matrix profile payload for backend strategy dispatch
  return(matrix_profile)
}
####
## End
# 
