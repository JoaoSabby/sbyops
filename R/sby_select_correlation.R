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
#' @details
#' This function computes absolute Pearson correlation for selected
#' numeric columns and iteratively removes columns until all active
#' pairwise correlations are below `threshold`
#'
#' @section Automatic engine rule:
#' Engine selection uses `prod(dim(selected_data))` from data after
#' tidyselect filtering. Streaming is used up to the configured
#' threshold, Fortran above that threshold, and BLAS with OpenMP for
#' very large workloads
#'
#' During heavy execution paths (`fortran` and `blas` strategies), this function
#' applies a temporary thread context derived from `sby_config_max_threads` and
#' restores the original environment and R options on exit.
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
#' @examples
#' # Prepare numeric columns to evaluate pairwise correlation
#' sample_data <- data.frame(a = 1:5, b = 2:6, c = c(1, 0, 1, 0, 1))
#'
#' # Apply correlation filtering with explicit threshold argument
#' sby_select_correlation(
#'   .data = sample_data,
#'   threshold = 0.9
#' )
#' @export
sby_select_correlation <- function(.data, ..., threshold){

  # Validate tabular input for supported data structure classes
  sby_internal_validate_tabular_input(.data = .data)

  # Validate threshold contract for correlation filtering
  threshold <- sby_internal_validate_correlation_threshold(
    threshold = threshold
  )

  # Return unchanged input when no rows or columns are available
  if(fncol(.data) == 0L || fnrow(.data) == 0L){

    # Return input unchanged for empty tabular shapes
    return(.data)
  }

  # Resolve and normalize column names for deterministic selection
  resolved_names <- sby_internal_resolve_column_names(.data = .data)
  colnames(.data) <- resolved_names

  # Resolve selected columns using numeric default policy
  selected_columns <- sby_internal_eval_select(
    .data = .data,
    ...,
    default = "numeric"
  )

  # Return unchanged input when no columns are selected
  if(length(selected_columns) == 0L){

    # Return input unchanged when selection is empty
    return(.data)
  }

  # Filter selected data and keep only numeric vectors
  selected_data <- .data[, unname(selected_columns), drop = FALSE]
  numeric_mask <- vapply(
    X = as.data.frame(selected_data),
    FUN = sby_internal_is_numeric_column,
    FUN.VALUE = logical(1L)
  )

  # Return unchanged input when fewer than two numeric columns remain
  if(sum(numeric_mask) < 2L){

    # Return input unchanged when correlation is not computable
    return(.data)
  }

  # Materialize numeric matrix in double precision
  numeric_data <- selected_data[, numeric_mask, drop = FALSE]
  numeric_column_names <- colnames(numeric_data)
  numeric_matrix <- data.matrix(numeric_data)
  storage.mode(numeric_matrix) <- "double"

  # Select automatic strategy using configured thresholds
  selected_strategy <- sby_internal_select_correlation_strategy(
    selected_data = numeric_matrix
  )

  requested_threads <- sby_internal_get_max_threads()
  context <- sby_internal_capture_thread_context(
    useOpenmp = selected_strategy %in% c("fortran", "blas"),
    useBlas = selected_strategy == "blas"
  )
  on.exit(sby_internal_restore_thread_context(context), add = TRUE)

  if(selected_strategy %in% c("fortran", "blas")){
    sby_internal_apply_thread_context(
      maxThreads = requested_threads,
      threadContext = context,
      useOpenmp = TRUE,
      useBlas = selected_strategy == "blas"
    )
  }

  # Dispatch correlation computation according to selected strategy
  if(selected_strategy == "fortran"){
    correlation_matrix <- sby_internal_compute_correlation_fortran(
      numeric_matrix = numeric_matrix
    )
  } else if(selected_strategy == "blas"){
    correlation_matrix <- sby_internal_compute_correlation_blas(mat = numeric_matrix)
  } else {
    correlation_matrix <- sby_internal_compute_correlation_streaming(mat = numeric_matrix)
  }

  # Compose concise execution message with backend and thread details
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

  # Apply iterative correlation pruning using computed matrix
  removed_columns <- sby_internal_apply_correlation_selection(
    cor_mat = correlation_matrix,
    threshold = threshold
  )

  # Build filtered dataset after removing selected correlated columns
  kept_columns <- setdiff(colnames(.data), removed_columns)
  filtered_data <- .data[, kept_columns, drop = FALSE]
  restored_data <- sby_internal_restore_selected_data(
    selected_data = filtered_data,
    original = .data
  )

  # Return filtered data restored to input structural class
  return(restored_data)
}
####
## End
# 
