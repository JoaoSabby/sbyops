# Internal helpers for sbyops.

sby_int_validate_data_frame <- function(.data) {
  if (!inherits(.data, "data.frame")) {
    stop("`.data` must be a data.frame or tibble.", call. = FALSE)
  }
  invisible(.data)
}

sby_int_validate_threshold <- function(threshold) {
  if (missing(threshold)) {
    stop("`threshold` is required.", call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold)) {
    stop("`threshold` must be a non-missing numeric scalar.", call. = FALSE)
  }
  if (!is.finite(threshold)) {
    stop("`threshold` must be finite.", call. = FALSE)
  }
  if (threshold < 0 || threshold > 1) {
    stop("`threshold` must be between 0 and 1.", call. = FALSE)
  }
  as.numeric(threshold)
}

sby_int_eval_select <- function(.data, ..., default = c("all", "numeric")) {
  default <- match.arg(default)
  dots <- rlang::enquos(...)
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
  list(
    codes = as.integer(f),
    max_code = nlevels(f)
  )
}

sby_int_restore_data_frame_attributes <- function(out, original) {
  if (inherits(original, "tbl_df") && requireNamespace("tibble", quietly = TRUE)) {
    out <- tibble::as_tibble(out)
  }
  out
}

sby_int_correlation_matrix_is_safe <- function(p) {
  estimated_bytes <- as.numeric(p) * as.numeric(p) * 8
  max_bytes <- getOption("sbyops.correlation.max_bytes", 2 * 1024^3)
  is.finite(estimated_bytes) && estimated_bytes <= max_bytes
}
