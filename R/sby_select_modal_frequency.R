#' Select columns by modal frequency
#'
#' `sby_select_modal_frequency()` removes selected columns whose most frequent
#' value appears in a proportion greater than or equal to `threshold`.
#'
#' @param .data A data.frame, tibble, or matrix.
#' @param ... Tidyselect column selectors. If empty, all supported columns are
#'   evaluated. Matrix input evaluates all columns.
#' @param threshold Numeric scalar between 0 and 1.
#'
#' @return `.data` with high modal-frequency columns removed.
#'
#' @export
sby_select_modal_frequency <- function(.data, ..., threshold) {
  sby_int_validate_tabular_input(.data)
  threshold <- sby_int_validate_modal_frequency_threshold(threshold)

  if (ncol(.data) == 0L || nrow(.data) == 0L) {
    return(.data)
  }

  resolved_names <- sby_int_resolve_column_names(.data)
  colnames(.data) <- resolved_names

  selected <- sby_int_eval_select(.data, ..., default = "all")
  if (length(selected) == 0L) {
    return(.data)
  }

  selected_data <- .data[, unname(selected), drop = FALSE]
  supported <- vapply(as.data.frame(selected_data), sby_int_is_modal_supported, logical(1))
  if (!any(supported)) {
    return(.data)
  }

  selected_data <- selected_data[, supported, drop = FALSE]
  selected_names <- colnames(selected_data)

  encoded <- lapply(as.data.frame(selected_data), sby_int_encode_modal_column)
  codes <- lapply(encoded, `[[`, "codes")
  max_codes <- vapply(encoded, `[[`, integer(1), "max_code")

  native <- .Call(
    "sby_modal_frequency_codes_fortran",
    codes,
    as.integer(max_codes),
    PACKAGE = "sbyops"
  )
  names(native) <- c("column_index", "ratio", "code", "count")

  removed <- selected_names[native$column_index[native$ratio >= threshold]]
  keep <- setdiff(colnames(.data), removed)
  out <- .data[, keep, drop = FALSE]
  sby_int_restore_selected_data(out, .data)
}
