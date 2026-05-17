#' Select columns by Pearson correlation
#'
#' `sby_select_correlation()` removes highly correlated numeric columns. In each
#' pair with absolute Pearson correlation greater than or equal to `threshold`,
#' the function removes the column with the larger mean absolute correlation
#' against the still-active candidate columns.
#'
#' @param .data A data.frame or tibble.
#' @param ... Tidyselect column selectors. If empty, all numeric columns are
#'   evaluated.
#' @param threshold Numeric scalar between 0 and 1.
#'
#' @return `.data` with highly correlated selected columns removed.
#'
#' @export
sby_select_correlation <- function(.data, ..., threshold) {
  sby_int_validate_data_frame(.data)
  threshold <- sby_int_validate_threshold(threshold)

  if (ncol(.data) == 0L || nrow(.data) == 0L) {
    return(.data)
  }

  selected <- sby_int_eval_select(.data, ..., default = "numeric")
  if (length(selected) == 0L) {
    return(.data)
  }

  selected_data <- .data[, unname(selected), drop = FALSE]
  numeric <- vapply(selected_data, sby_int_is_numeric_column, logical(1))
  if (sum(numeric) < 2L) {
    return(.data)
  }

  numeric_data <- selected_data[, numeric, drop = FALSE]
  numeric_names <- names(numeric_data)
  p <- ncol(numeric_data)

  if (!sby_int_correlation_matrix_is_safe(p)) {
    stop(
      "The selected columns would require a correlation matrix that is too large ",
      "for this initial implementation. Select fewer columns before calling ",
      "`sby_select_correlation()`.",
      call. = FALSE
    )
  }

  mat <- data.matrix(numeric_data)
  storage.mode(mat) <- "double"

  native <- .Call(
    "sby_correlation_pearson_matrix_fortran",
    mat,
    as.integer(nrow(mat)),
    as.integer(ncol(mat)),
    PACKAGE = "sbyops"
  )
  cor_mat <- matrix(native, nrow = p, ncol = p)
  dimnames(cor_mat) <- list(numeric_names, numeric_names)

  removed <- sby_int_correlated_columns_to_remove(cor_mat, threshold)
  keep <- setdiff(names(.data), removed)
  out <- .data[, keep, drop = FALSE]
  sby_int_restore_data_frame_attributes(out, .data)
}

sby_int_correlated_columns_to_remove <- function(cor_mat, threshold) {
  if (ncol(cor_mat) < 2L) {
    return(character())
  }

  diag(cor_mat) <- 0
  cor_mat[!is.finite(cor_mat)] <- 0

  active <- rep(TRUE, ncol(cor_mat))
  removed <- character()
  column_names <- colnames(cor_mat)

  repeat {
    active_idx <- which(active)
    if (length(active_idx) < 2L) {
      break
    }

    active_cor <- cor_mat[active_idx, active_idx, drop = FALSE]
    max_cor_mat <- active_cor
    diag(max_cor_mat) <- -Inf
    max_cor <- max(max_cor_mat)
    if (!is.finite(max_cor) || max_cor < threshold) {
      break
    }

    pair_pos <- which(max_cor_mat == max_cor, arr.ind = TRUE)[1L, ]
    i <- active_idx[pair_pos[1L]]
    j <- active_idx[pair_pos[2L]]

    mean_cor <- active_cor
    diag(mean_cor) <- 0
    mean_abs <- rowMeans(mean_cor)
    mean_i <- mean_abs[pair_pos[1L]]
    mean_j <- mean_abs[pair_pos[2L]]

    if (mean_i >= mean_j) {
      remove_idx <- i
    } else {
      remove_idx <- j
    }

    active[remove_idx] <- FALSE
    removed <- c(removed, column_names[remove_idx])
  }

  removed
}
