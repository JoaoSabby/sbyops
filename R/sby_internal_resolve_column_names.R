#' @title Internal Helper: Resolve Column Names
#'
#' @usage sby_internal_resolve_column_names(.data)
#'
#' @description Normalize and repair column names for deterministic downstream selection behavior
#'
#' @details This helper trims whitespace, imputes missing names with sequential placeholders, and guarantees uniqueness by suffixing duplicates
#'
#' @section Internal contract:
#' This function expects a tabular object with a defined column count
#'
#' @param .data A data frame, tibble, or matrix
#'
#' @return A character vector with normalized unique column names
#'
#' @seealso [sby_select_correlation()], [sby_select_modal_frequency()], [sby_select_non_constant()]
#'
#' @references R CORE TEAM. Writing R Extensions. Vienna: R Foundation. Available at: \href{https://cran.r-project.org/doc/manuals/r-release/R-exts.html}{R Extensions Manual}
#'
#' @examples
#' exampleFrame <- data.frame(" " = 1:2, check.names = FALSE)
#' sby_internal_resolve_column_names(exampleFrame)
sby_internal_resolve_column_names <- function(.data){

  # Read raw column metadata used for deterministic name reconstruction
  column_count <- ncol(.data)
  existing_names <- colnames(.data)

  # Build default names when no column names exist in input data
  if(is.null(existing_names)){
    generated_names <- sprintf("v%03d", seq_len(column_count))
    return(generated_names)
  }

  # Trim whitespace and identify missing or blank entries for replacement
  repaired_names <- trimws(existing_names)
  missing_mask <- is.na(repaired_names) | repaired_names == ""

  # Replace missing names by explicit positional labels without using which
  missing_counter <- cumsum(missing_mask)
  repaired_names[missing_mask] <- sprintf("v%03d", missing_counter[missing_mask])

  # Enforce uniqueness to stabilize selectors and downstream joins
  unique_names <- make.unique(repaired_names, sep = "__")

  # Return normalized name vector for caller assignment
  return(unique_names)
}
####
## End
# 
