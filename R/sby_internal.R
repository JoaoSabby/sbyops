# Internal helpers for sbyops.

sby_int_validate_tabular_input <- function(.data) {
  if (!(inherits(.data, "data.frame") || is.matrix(.data))) {
    stop("`.data` must be a data.frame, tibble, or matrix.", call. = FALSE)
  }
  invisible(.data)
}

sby_int_validate_modal_frequency_threshold <- function(threshold) {
  sby_int_validate_threshold_scalar(threshold, "threshold")
}

sby_int_validate_correlation_threshold <- function(threshold) {
  sby_int_validate_threshold_scalar(threshold, "threshold")
}

sby_int_validate_threshold_scalar <- function(threshold, arg_name) {
  if (missing(threshold)) {
    stop(sprintf("`%s` is required.", arg_name), call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold)) {
    stop(sprintf("`%s` must be a non-missing numeric scalar.", arg_name), call. = FALSE)
  }
  if (!is.finite(threshold)) {
    stop(sprintf("`%s` must be finite.", arg_name), call. = FALSE)
  }
  if (threshold < 0 || threshold > 1) {
    stop(sprintf("`%s` must be between 0 and 1.", arg_name), call. = FALSE)
  }
  as.numeric(threshold)
}

sby_int_resolve_column_names <- function(.data) {
  n_cols <- if (is.matrix(.data)) ncol(.data) else ncol(.data)
  existing_names <- colnames(.data)
  if (is.null(existing_names)) {
    return(sprintf("v%03d", seq_len(n_cols)))
  }

  repaired <- trimws(existing_names)
  repaired[is.na(repaired) | repaired == ""] <- sprintf("v%03d", which(is.na(repaired) | repaired == ""))
  make.unique(repaired, sep = "__")
}

sby_int_eval_select <- function(.data, ..., default = c("all", "numeric")) {
  default <- match.arg(default)
  dots <- rlang::enquos(...)

  if (is.matrix(.data)) {
    if (length(dots) > 0L) {
      stop("Tidyselect columns are not supported for matrix inputs.", call. = FALSE)
    }
    return(seq_len(ncol(.data)))
  }

  if (length(dots) == 0L) {
    if (default == "numeric") {
      selected <- which(vapply(.data, function(x) is.null(dim(x)) && is.numeric(x), logical(1)))
    } else {
      selected <- seq_along(.data)
    }
    names(selected) <- names(.data)[selected]
    return(selected)
  }
  tidyselect::eval_select(rlang::expr(c(!!!dots)), .data)
}

sby_int_is_modal_supported <- function(x) {
  is.null(dim(x)) && (
    is.factor(x) || is.character(x) || is.integer(x) ||
      is.logical(x) || is.numeric(x)
  )
}

sby_int_is_numeric_column <- function(x) {
  is.null(dim(x)) && is.numeric(x)
}

sby_int_encode_modal_column <- function(x) {
  f <- factor(x, exclude = NULL)
  list(codes = as.integer(f), max_code = nlevels(f))
}

sby_int_estimate_correlation_memory <- function(n_rows, n_cols) {
  matrix_bytes <- as.numeric(n_rows) * as.numeric(n_cols) * 8
  corr_bytes <- as.numeric(n_cols) * as.numeric(n_cols) * 8
  list(matrix_bytes = matrix_bytes, corr_bytes = corr_bytes)
}

sby_int_inspect_matrix_profile <- function(mat) {
  n_rows <- nrow(mat)
  n_cols <- ncol(mat)
  memory <- sby_int_estimate_correlation_memory(n_rows, n_cols)
  has_non_finite <- any(!is.finite(mat))
  n_pairs <- (n_cols * (n_cols - 1L)) / 2L
  list(
    n_rows = n_rows,
    n_cols = n_cols,
    n_pairs = n_pairs,
    has_non_finite = has_non_finite,
    matrix_bytes = memory$matrix_bytes,
    corr_bytes = memory$corr_bytes
  )
}

sby_int_select_correlation_strategy <- function(profile) {
  max_corr_bytes <- getOption("sbyops.correlation.max_bytes", 2 * 1024^3)
  max_centered_bytes <- getOption("sbyops.correlation.max_centered_bytes", 1 * 1024^3)
  large_rows <- profile$n_rows >= getOption("sbyops.correlation.large_rows", 500000L)

  if (profile$corr_bytes > max_corr_bytes) {
    stop(
      "The selected columns would require a correlation matrix that is too large ",
      "for this implementation. Select fewer columns before calling ",
      "`sby_select_correlation()`.",
      call. = FALSE
    )
  }

  if (!profile$has_non_finite && profile$n_rows > 1L) {
    centered_bytes <- profile$matrix_bytes
    if (centered_bytes <= max_centered_bytes && (large_rows || profile$n_pairs >= 2000L)) {
      return("blas")
    }
    if (large_rows) {
      return("streaming")
    }
  }

  "fortran"
}

sby_int_compute_correlation_fortran <- function(mat) {
  native <- .Call(
    "sby_correlation_pearson_matrix_fortran",
    mat,
    as.integer(nrow(mat)),
    as.integer(ncol(mat)),
    PACKAGE = "sbyops"
  )
  matrix(native, nrow = ncol(mat), ncol = ncol(mat))
}

sby_int_compute_correlation_blas <- function(mat) {
  n_rows <- nrow(mat)
  means <- colMeans(mat)
  centered <- sweep(mat, 2L, means, FUN = "-")
  ss <- colSums(centered * centered)
  denom <- sqrt(outer(ss, ss))
  cov_mat <- crossprod(centered)
  cor_mat <- abs(cov_mat / denom)
  cor_mat[!is.finite(cor_mat)] <- 0
  diag(cor_mat) <- 0
  cor_mat
}

sby_int_compute_correlation_streaming <- function(mat) {
  # Caminho de baixa memória: evita a cópia de centralização completa.
  n_rows <- nrow(mat)
  n_cols <- ncol(mat)
  means <- colMeans(mat)
  ss <- numeric(n_cols)
  for (j in seq_len(n_cols)) {
    centered_col <- mat[, j] - means[j]
    ss[j] <- sum(centered_col * centered_col)
  }

  cov_mat <- matrix(0, n_cols, n_cols)
  block_rows <- max(1024L, min(n_rows, as.integer(getOption("sbyops.correlation.block_rows", 50000L))))
  start <- 1L
  while (start <= n_rows) {
    end <- min(start + block_rows - 1L, n_rows)
    block <- mat[start:end, , drop = FALSE]
    block_centered <- sweep(block, 2L, means, FUN = "-")
    cov_mat <- cov_mat + crossprod(block_centered)
    start <- end + 1L
  }

  denom <- sqrt(outer(ss, ss))
  cor_mat <- abs(cov_mat / denom)
  cor_mat[!is.finite(cor_mat)] <- 0
  diag(cor_mat) <- 0
  cor_mat
}

sby_int_apply_correlation_selection <- function(cor_mat, threshold) {
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

    remove_idx <- if (mean_i >= mean_j) i else j
    active[remove_idx] <- FALSE
    removed <- c(removed, column_names[remove_idx])
  }

  removed
}

sby_int_restore_selected_data <- function(out, original) {
  if (is.matrix(original)) {
    if (!is.null(rownames(original))) {
      rownames(out) <- rownames(original)
    }
    return(as.matrix(out))
  }

  if (inherits(original, "tbl_df") && requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(out))
  }
  out
}
