#' @title Internal Helper: Apply Correlation Selection
#'
#' @usage sby_internal_apply_correlation_selection(cor_mat, threshold)
#'
#' @description
#' Apply iterative high-correlation pruning over an absolute
#' correlation matrix
#'
#' @details
#' This helper repeatedly finds the highest active pairwise
#' correlation and removes one variable from the pair according to
#' larger mean absolute correlation against active variables
#'
#' @section Internal contract:
#' This function expects a square numeric matrix with column names
#' and a validated threshold in `[0, 1]`
#'
#' @param cor_mat A square numeric absolute-correlation matrix
#'
#' @param threshold A numeric scalar in `[0, 1]`
#'
#' @return A character vector containing the names of removed columns
#'
#' @seealso [sby_select_correlation()]
#'
#' @references
#' JOLLIFFE, I. T. Principal Component Analysis. 2. ed. New York:
#' Springer, 2002. DOI:10.1007/b98835.
#' https://doi.org/10.1007/b98835
#'
#' @examples
#' example_correlation <- matrix(c(0, 0.9, 0.9, 0), nrow = 2)
#' colnames(example_correlation) <- c("a", "b")
#' sby_internal_apply_correlation_selection(
#'   cor_mat = example_correlation,
#'   threshold = 0.8
#' )
sby_internal_apply_correlation_selection <- function(cor_mat, threshold){

  # Initialize removed-column container for deterministic return shape
  removed_columns <- character()

  # Return early when no pairwise comparison is possible
  if(ncol(cor_mat) < 2L){

    # Return removed column names for degenerate matrix shapes
    return(removed_columns)
  }

  # Sanitize diagonal and non-finite entries to avoid artifacts
  diag(cor_mat) <- 0
  cor_mat[!is.finite(cor_mat)] <- 0

  # Track active columns and preserve name lookup for output mapping
  active_mask <- rep(TRUE, ncol(cor_mat))
  column_names <- colnames(cor_mat)

  # Iterate until no active pair violates the correlation threshold
  repeat{

    # Extract active indexes to build the current working submatrix
    active_indexes <- seq_along(active_mask)[active_mask]

    # Stop when fewer than two columns remain active
    if(length(active_indexes) < 2L){
      break
    }

    # Build the active absolute-correlation submatrix
    active_correlation <- cor_mat[active_indexes, active_indexes, drop = FALSE]

    # Ignore diagonal self-correlation before max-pair extraction
    diag(active_correlation) <- -Inf

    # Compute the largest active pairwise absolute correlation
    max_correlation <- max(active_correlation)

    # Stop when threshold condition is no longer violated
    if(!is.finite(max_correlation) || max_correlation < threshold){
      break
    }

    # Locate every pair tied at the maximum active correlation
    max_pair_matrix <- active_correlation == max_correlation

    # Keep only one triangle to avoid symmetric duplicated pairs
    max_pair_matrix[lower.tri(max_pair_matrix, diag = TRUE)] <- FALSE

    # Convert matrix mask to coordinate table without iterative loops
    max_pair_table <- as.data.frame(as.table(max_pair_matrix))

    # Keep only coordinates marked as TRUE in the maximum mask
    max_pair_table <- max_pair_table[max_pair_table$Freq, , drop = FALSE]

    # Select the first deterministic tied pair for reproducibility
    pair_row <- as.integer(max_pair_table$Var1[1L])
    pair_col <- as.integer(max_pair_table$Var2[1L])

    # Map pair coordinates back to original matrix column indexes
    first_index <- active_indexes[pair_row]
    second_index <- active_indexes[pair_col]

    # Copy active matrix to compute tie-breaking correlation burden
    mean_correlation <- active_correlation

    # Replace diagonal with zero before row-wise mean aggregation
    diag(mean_correlation) <- 0

    # Compute mean absolute correlation burden for each active column
    mean_absolute_correlation <- rowMeans(mean_correlation)

    # Read burden values for both candidate columns in the tied pair
    first_mean <- mean_absolute_correlation[pair_row]
    second_mean <- mean_absolute_correlation[pair_col]

    # Remove the column with higher mean absolute burden
    if(first_mean >= second_mean){
      remove_index <- first_index
    } else {
      remove_index <- second_index
    }

    # Mark removed column as inactive for next iteration pass
    active_mask[remove_index] <- FALSE

    # Append removed column name to ordered output container
    removed_columns <- c(removed_columns, column_names[remove_index])
  }

  # Return complete removed-column list after convergence
  return(removed_columns)
}
####
## End
# 
