#' @title Select Columns by Pearson Correlation
#'
#' @usage
#' sby_select_correlation(
#'   .data,
#'   ...,
#'   threshold
#' )
#'
#' @description
#' Remove selected numeric columns that exceed a Pearson correlation
#' threshold
#'
#' @param .data A data frame, tibble, or numeric-compatible matrix
#'
#' @param ... Tidyselect expressions for data-frame-like inputs
#'
#' @param threshold A numeric scalar in `[0, 1]`
#'
#' @return Object with the same structure as `.data` and filtered columns
#'
#' @seealso [sby_select_modal_frequency()], [sby_select_non_constant()], [sby_config()]
#'
#' @importFrom cli cli_alert_info
#'
#' @examples
#' sample_data <- data.frame(a = 1:5, b = 2:6, c = c(1, 0, 1, 0, 1))
#' sby_select_correlation(.data = sample_data, threshold = 0.9)
#' @export
sby_select_correlation <- function(.data, ..., threshold){

  if(fncol(.data) == 0L || fnrow(.data) == 0L) return(.data)

  selected_columns <- sby_internal_eval_select(
    .data = .data,
    ...,
    default = "numeric"
  )

  if(length(selected_columns) == 0L) return(.data)

  numeric_data <- .data[, unname(selected_columns), drop = FALSE]

  if(fncol(numeric_data) < 2L) return(.data)

  numeric_matrix <- data.matrix(numeric_data)
  storage.mode(numeric_matrix) <- "double"

  selected_strategy <- sby_internal_select_correlation_strategy(
    selected_data = numeric_matrix
  )
  if(any(!is.finite(numeric_matrix))){
    selected_strategy <- "fortran"
  }

  requested_threads <- sby_internal_get_max_threads()
  context <- sby_internal_capture_thread_context(
    useOpenmp = selected_strategy %in% c("fortran", "blas"),
    useBlas   = selected_strategy == "blas"
  )
  on.exit(sby_internal_restore_thread_context(context), add = TRUE)

  if(selected_strategy %in% c("fortran", "blas")){
    sby_internal_apply_thread_context(
      maxThreads    = requested_threads,
      threadContext = context,
      useOpenmp     = TRUE,
      useBlas       = selected_strategy == "blas"
    )
  }

  if(selected_strategy == "fortran"){
    removed_columns <- sby_internal_compute_correlation_fortran(
      numeric_matrix = numeric_matrix,
      threshold      = threshold
    )
  } else if(selected_strategy == "blas"){
    correlation_matrix <- sby_internal_compute_correlation_blas(mat = numeric_matrix)
    removed_columns <- sby_internal_apply_correlation_selection(
      cor_mat   = correlation_matrix,
      threshold = threshold
    )
  } else {
    correlation_matrix <- sby_internal_compute_correlation_streaming(mat = numeric_matrix)
    removed_columns <- sby_internal_apply_correlation_selection(
      cor_mat   = correlation_matrix,
      threshold = threshold
    )
  }

  blas_library <- extSoftVersion()["BLAS"]
  rhpc_used <- !is.null(context$rhpc) && isTRUE(context$rhpc$used)
  cli::cli_alert_info(
    paste0(
      "strategy=", selected_strategy,
      " | BLAS_detected=", blas_library,
      " | threads_requested=", requested_threads,
      " | RhpcBLASctl_used=", ifelse(rhpc_used, "yes", "no"),
      " | context_restore=enabled"
    )
  )

  kept_columns <- setdiff(colnames(.data), removed_columns)
  return(.data[, kept_columns, drop = FALSE])
}
####
## End
# 
