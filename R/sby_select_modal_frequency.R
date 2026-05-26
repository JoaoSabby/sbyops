#' @title Select Columns by Modal Frequency
#'
#' @usage sby_select_modal_frequency(.data, ..., threshold)
#'
#' @description Remove selected columns whose most frequent value proportion is greater than or equal to a threshold
#'
#' @details Columns are encoded as integer codes and evaluated in native Fortran for efficient modal-frequency estimation
#'
#' @param .data A data frame, tibble, or matrix
#'
#' @param ... Tidyselect expressions for data-frame-like inputs. When omitted, all supported columns are considered
#'
#' @param threshold A numeric scalar in the closed interval `[0, 1]`
#'
#' @return An object with the same structural class as `.data` with high modal-frequency columns removed
#'
#' @seealso [sby_select_correlation()], [sby_select_non_constant()]
#'
#' @references
#'
#' @examples
#' modalData <- data.frame(a = c("x", "x", "x", "y"), b = c(1, 2, 3, 4))
#' sby_select_modal_frequency(modalData, threshold = 0.75)
#' @export
sby_select_modal_frequency <- function(.data, ..., threshold){

  sby_internal_validate_tabular_input(.data = .data)
  threshold <- sby_internal_validate_threshold_scalar(
    threshold = threshold,
    arg_name = "threshold"
  )

  if(ncol(.data) == 0L || nrow(.data) == 0L){
    return(.data)
  }

  resolved_names <- sby_internal_resolve_column_names(.data = .data)
  colnames(.data) <- resolved_names

  selected_columns <- sby_internal_eval_select(
    .data = .data,
    ...,
    default = "all"
  )
  if(length(selected_columns) == 0L){
    return(.data)
  }

  selected_data <- .data[, unname(selected_columns), drop = FALSE]
  supported_mask <- vapply(
    X = as.data.frame(selected_data),
    FUN = sby_internal_is_modal_supported,
    FUN.VALUE = logical(1L)
  )
  if(!any(supported_mask)){
    return(.data)
  }

  supported_data <- selected_data[, supported_mask, drop = FALSE]
  supported_column_names <- colnames(supported_data)
  encoded_columns <- lapply(
    X = as.data.frame(supported_data),
    FUN = sby_internal_encode_modal_column
  )
  column_codes <- lapply(X = encoded_columns, FUN = `[[`, "codes")
  column_max_codes <- vapply(X = encoded_columns, FUN = `[[`, FUN.VALUE = integer(1), "max_code")

  native_result <- .Call(
    .NAME = "sby_modal_frequency_codes_fortran",
    codes = column_codes,
    max_codes = as.integer(column_max_codes),
    PACKAGE = "sbyops"
  )
  names(native_result) <- c("column_index", "ratio", "code", "count")

  removed_columns <- supported_column_names[native_result$column_index[native_result$ratio >= threshold]]
  kept_columns <- setdiff(colnames(.data), removed_columns)
  filtered_data <- .data[, kept_columns, drop = FALSE]
  sby_internal_restore_selected_data(selected_data = filtered_data, original = .data)
}
####
## End
# 
