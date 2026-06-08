#' @title Select Columns by Modal Frequency
#'
#' @usage
#' sby_select_modal_frequency(
#'   .data,
#'   threshold
#' )
#'
#' @description Remove selected columns whose most frequent value proportion is greater than or equal to a threshold
#'
#' @details Uses a native type-specialized backend for logical, integer/factor, numeric, and character columns.
#'
#' @param .data A data frame or tibble
#'
#' @param threshold A numeric scalar in the closed interval `[0, 1]`
#'
#' @return An object with the same structural class as `.data` with high modal-frequency columns removed
#' @export
#' Filtra colunas com base na frequencia modal
#'
#' @param .data dados
#' @param threshold limite de frequencia
#'
#' @returns
#' @export
#'
#' @examples
sby_select_modal_frequency <- function(.data, threshold = 0.99){

  # Validate supported tabular input and threshold before scanning columns
  sby_internal_validate_tabular_input(.data = .data)
  threshold <- sby_internal_validate_threshold_scalar(
    threshold = threshold,
    arg_name = "threshold"
  )

  # Cache dimensions once because they are reused by both R and native paths
  n_cols <- collapse::fncol(.data)
  n_rows <- collapse::fnrow(.data)

  # Return unchanged input for degenerate tabular shapes
  if(n_cols == 0L || n_rows == 0L){
    return(.data)
  }

  # Normalize names for deterministic column removal and restoration
  resolved_names <- sby_internal_resolve_column_names(.data = .data)
  colnames(.data) <- resolved_names

  # Materialize a plain column list once before dispatching to the fast backend
  column_data <- as.data.frame(.data, stringsAsFactors = FALSE)
  cutoff <- ceiling(threshold * n_rows)

  # Use the registered native type-specialized mask; unsupported columns fall back to R
  keep_mask <- .Call(
    "sby_internal_modal_frequency_keep_mask",
    column_data,
    threshold,
    as.integer(n_rows),
    PACKAGE = "sbyops"
  )

  # Fallback for unsupported atomic/list-like columns keeps exact current semantics
  fallback_columns <- which(is.na(keep_mask))
  if(length(fallback_columns) > 0L){
    count_occur <- kit::countOccur

    keep_mask[fallback_columns] <- vapply(
      X = column_data[fallback_columns],
      FUN = function(current_column){
        occurrence_table <- count_occur(current_column)

        if(nrow(occurrence_table) == 0L){
          return(TRUE)
        }

        max_count <- max(as.numeric(occurrence_table[, 2L]), na.rm = TRUE)
        max_count < cutoff
      },
      FUN.VALUE = logical(1L)
    )
  }

  # Remove columns whose modal proportion reaches or exceeds the threshold
  kept_columns <- names(column_data)[keep_mask]
  filtered_data <- .data[, kept_columns, drop = FALSE]

  # Restore output class to match the original input structure
  return(
    sby_internal_restore_selected_data(
      selected_data = filtered_data,
      original = .data
    )
  )
}
####
## End
#
