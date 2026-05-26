#' @title Internal Helper: Restore Selected Data
#'
#' @usage sby_internal_restore_selected_data(selected_data, original)
#'
#' @description
#' Restore filtered tabular data to a structure compatible with the
#' original input class
#'
#' @details
#' Matrix inputs are restored as matrices with row names when present.
#' Tibble inputs are restored as tibbles when the namespace is present.
#' All other inputs return a base data frame
#'
#' @param selected_data Filtered tabular object produced by selectors
#'
#' @param original Original input object used to infer output class
#'
#' @return A restored tabular object compatible with original class
#'
#' @seealso [sby_select_correlation()], [sby_select_modal_frequency()]
#'
#' @references
#' R CORE TEAM. Writing R Extensions. Vienna: R Foundation.
#' \href{https://cran.r-project.org/doc/manuals/r-release/R-exts.html}{R Extensions Manual}
#'
#' @examples
#' original_data <- data.frame(a = 1:3, b = 3:1)
#' filtered_data <- original_data[, "a", drop = FALSE]
#' sby_internal_restore_selected_data(filtered_data, original_data)
sby_internal_restore_selected_data <- function(selected_data, original){

  # Rebuild matrix outputs while preserving row-name metadata
  if(is.matrix(original)){
    if(!is.null(rownames(original))){
      rownames(selected_data) <- rownames(original)
    }

    # Materialize matrix result before explicit return
    restored_matrix <- as.matrix(selected_data)

    # Return restored matrix with preserved row-name metadata
    return(restored_matrix)
  }

  # Rebuild tibble outputs only when tibble namespace is available
  if(inherits(original, "tbl_df") && requireNamespace("tibble", quietly = TRUE)){

    # Materialize tibble result before explicit return
    restored_tibble <- tibble::as_tibble(selected_data)

    # Return restored tibble compatible with original class
    return(restored_tibble)
  }

  # Materialize base data-frame output for remaining classes
  restored_data_frame <- selected_data

  # Return base data-frame output for remaining object classes
  return(restored_data_frame)
}
####
## End
# 
