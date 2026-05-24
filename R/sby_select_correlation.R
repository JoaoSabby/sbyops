#' Select columns by Pearson correlation
#'
#' `sby_select_correlation()` removes highly correlated numeric columns. In each
#' pair with absolute Pearson correlation greater than or equal to `threshold`,
#' the function removes the column with the larger mean absolute correlation
#' against the still-active candidate columns.
#'
#' A estratégia de execução é automática e interna:
#' caminho Fortran robusto, caminho BLAS para dados finitos com memória segura,
#' ou caminho streaming para reduzir cópias em matrizes altas.
#'
#' @param .data A data.frame, tibble, or matrix.
#' @param ... Tidyselect column selectors for data.frame/tibble. If empty, all
#'   numeric columns are evaluated. Matrix input evaluates all columns.
#' @param threshold Numeric scalar between 0 and 1.
#'
#' @return `.data` with highly correlated selected columns removed.
#'
#' @export
sby_select_correlation <- function(.data, ..., threshold) {
  sby_int_validate_tabular_input(.data)
  threshold <- sby_int_validate_correlation_threshold(threshold)

  if (ncol(.data) == 0L || nrow(.data) == 0L) {
    return(.data)
  }

  resolved_names <- sby_int_resolve_column_names(.data)
  colnames(.data) <- resolved_names

  selected <- sby_int_eval_select(.data, ..., default = "numeric")
  if (length(selected) == 0L) {
    return(.data)
  }

  selected_data <- .data[, unname(selected), drop = FALSE]
  numeric <- vapply(as.data.frame(selected_data), sby_int_is_numeric_column, logical(1))
  if (sum(numeric) < 2L) {
    return(.data)
  }

  numeric_data <- selected_data[, numeric, drop = FALSE]
  numeric_names <- colnames(numeric_data)

  mat <- data.matrix(numeric_data)
  storage.mode(mat) <- "double"

  profile <- sby_int_inspect_matrix_profile(mat)
  strategy <- sby_int_select_correlation_strategy(profile)

  cor_mat <- switch(
    strategy,
    fortran = sby_int_compute_correlation_fortran(mat),
    blas = sby_int_compute_correlation_blas(mat),
    streaming = sby_int_compute_correlation_streaming(mat),
    sby_int_compute_correlation_fortran(mat)
  )

  dimnames(cor_mat) <- list(numeric_names, numeric_names)

  removed <- sby_int_apply_correlation_selection(cor_mat, threshold)
  keep <- setdiff(colnames(.data), removed)
  out <- .data[, keep, drop = FALSE]
  sby_int_restore_selected_data(out, .data)
}
