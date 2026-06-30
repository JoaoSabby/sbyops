#' @title Select Columns by Pearson Correlation
#' @description Remove selected numeric columns that exceed a Pearson correlation threshold
#' @param .data A data frame or tibble
#' @param ... Tidyselect expressions
#' @param threshold A numeric scalar in `[0, 1]`
#' @param num_treads Optional positive integer scalar thread cap for this call.
#'   When supplied, it overrides `sby_config_max_threads` without changing the
#'   stored package configuration.
#' @return `.data` with correlated columns removed
#' @importFrom cli cli_alert_info
#' @export
sby_select_correlation <- function(.data, ..., threshold, num_treads = NULL){

  sby_internal_validate_tabular_input(.data = .data)
  threshold <- sby_internal_validate_correlation_threshold(threshold = threshold)

  selected_columns <- sby_internal_eval_select(.data = .data, ..., default = "numeric")

  selected_data <- .data[, unname(selected_columns), drop = FALSE]
  sby_internal_validate_tabular_input(
    .data = selected_data,
    validate_column_types = TRUE
  )

  numeric_matrix <- data.matrix(selected_data)
  storage.mode(numeric_matrix) <- "double"

  selected_strategy <- sby_internal_select_correlation_strategy(selected_data = numeric_matrix)

  requested_threads <- if(is.null(num_treads)){
    sby_internal_get_max_threads()
  } else {
    sby_internal_validate_max_threads(num_treads)
  }
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
