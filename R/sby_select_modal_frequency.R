#' @title Select Columns by Modal Frequency
#'
#' @usage
#' sby_select_modal_frequency(
#'   .data,
#'   ...,
#'   threshold
#' )
#'
#' @description Remove selected columns whose most frequent value proportion is greater than or equal to a threshold
#'
#' @details Uses a native type-specialized backend for logical, integer/factor, numeric, and character columns.
#'
#' The native routine receives an explicit `max_threads` argument from `sby_config_max_threads`
#' and runs inside a temporary OpenMP thread context that is restored on exit.
#'
#' @param .data A data frame, tibble, or matrix
#'
#' @param ... Tidyselect expressions for data-frame-like inputs. When omitted, all supported columns are considered
#'
#' @param threshold A numeric scalar in the closed interval `[0, 1]`
#'
#' @return An object with the same structural class as `.data` with high modal-frequency columns removed
#' @export
sby_select_modal_frequency <- function(.data, ..., threshold){
  sby_internal_validate_tabular_input(.data = .data)
  threshold <- sby_internal_validate_threshold_scalar(threshold = threshold, arg_name = "threshold")

  if(fncol(.data) == 0L || fnrow(.data) == 0L) return(.data)

  colnames(.data) <- sby_internal_resolve_column_names(.data = .data)
  selected_columns <- sby_internal_eval_select(.data = .data, ..., default = "all")
  if(length(selected_columns) == 0L) return(.data)

  selected_data <- .data[, unname(selected_columns), drop = FALSE]
  selected_list <- as.list(as.data.frame(selected_data, stringsAsFactors = FALSE))
  supported_mask <- vapply(selected_list, sby_internal_is_modal_supported, logical(1L))
  
  if(!any(supported_mask)) return(.data)

  supported_names <- names(selected_list)[supported_mask]
  
  if(threshold <= 0){
    keep_mask <- !(colnames(.data) %in% supported_names)
    return(sby_internal_restore_selected_data(selected_data = .data[, keep_mask, drop = FALSE], original = .data))
  }

  max_threads <- sby_internal_get_max_threads()

  keep_supported <- sby_internal_with_thread_context(.Call(
    "sby_modal_frequency_mask",
    selected_list[supported_mask],
    as.double(threshold),
    max_threads,
    PACKAGE = "sbyops"
  ), maxThreads = max_threads, useOpenmp = TRUE, useBlas = FALSE)

  removed_supported <- supported_names[!keep_supported]
  keep_mask <- !(colnames(.data) %in% removed_supported)
  sby_internal_restore_selected_data(selected_data = .data[, keep_mask, drop = FALSE], original = .data)
}
