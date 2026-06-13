#' @title Select Columns by Pearson Correlation
#' @description Remove selected numeric columns that exceed a Pearson correlation threshold
#' @param .data A data frame or tibble
#' @param ... Tidyselect expressions
#' @param threshold A numeric scalar in `[0, 1]`
#' @return `.data` with correlated columns removed
#' @importFrom cli cli_alert_info
#' @export
sby_select_correlation <- function(.data, ..., threshold){

  selected_columns <- sby_internal_eval_select(.data = .data, ..., default = "numeric")

  numeric_matrix <- data.matrix(.data[, unname(selected_columns), drop = FALSE])
  storage.mode(numeric_matrix) <- "double"

  selected_strategy <- sby_internal_select_correlation_strategy(selected_data = numeric_matrix)

  requested_threads <- sby_internal_get_max_threads()
  context <- sby_internal_capture_thread_context(
    useOpenmp = selected_strategy == "fortran",
    useBlas   = selected_strategy == "fortran"
  )
  on.exit(sby_internal_restore_thread_context(context), add = TRUE)

  if(selected_strategy == "fortran"){
    sby_internal_apply_thread_context(
      maxThreads    = requested_threads,
      threadContext = context,
      useOpenmp     = TRUE,
      useBlas       = TRUE
    )

    removed_columns <- sby_internal_compute_correlation_fortran(
      numeric_matrix = numeric_matrix,
      threshold      = threshold
    )
  } else {
    removed_columns <- sby_internal_apply_correlation_selection(
      cor_mat   = sby_internal_compute_correlation_streaming(mat = numeric_matrix),
      threshold = threshold
    )
  }

  .data[, setdiff(colnames(.data), removed_columns), drop = FALSE]
}
