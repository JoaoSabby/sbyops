#' @title Internal Helper: Eval Select
#'
#' @usage sby_internal_eval_select(.data, ..., default = c("all", "numeric"))
#'
#' @description Resolve selected columns from tidyselect expressions or default selection policy
#'
#' @details This helper enforces matrix selector constraints and computes deterministic fallback selections for data-frame-like inputs when selectors are absent
#'
#' @section Internal contract:
#' This function should be invoked by validated selector entry points
#'
#' @param .data A data frame, tibble, or matrix
#'
#' @param ... Tidyselect expressions
#'
#' @param default Default policy used when selectors are omitted
#'
#' @return An integer index vector with names for selected columns
#'
#' @seealso [sby_select_correlation()], [sby_select_modal_frequency()], [sby_select_non_constant()]
#'
#' @references R CORE TEAM. Writing R Extensions. Vienna: R Foundation. Available at: \href{https://cran.r-project.org/doc/manuals/r-release/R-exts.html}{R Extensions Manual}
#'
#' @examples
#' exampleData <- data.frame(a = 1:3, b = letters[1:3])
#' sby_internal_eval_select(exampleData, default = "all")
sby_internal_eval_select <- function(.data, ..., default = c("all", "numeric")){

  # Resolve fallback mode and capture selectors lazily for tidy evaluation
  default_mode <- match.arg(default)
  selector_expressions <- rlang::enquos(...)

  # Enforce matrix constraint where tidyselect expressions are invalid
  if(is.matrix(.data)){
    if(length(selector_expressions) > 0L){
      stop("Tidyselect columns are not supported for matrix inputs.", call. = FALSE)
    }

    selected_indexes <- seq_len(ncol(.data))
    return(selected_indexes)
  }

  # Resolve default selections when explicit selectors are not provided
  if(length(selector_expressions) == 0L){
    if(default_mode == "numeric"){
      numeric_mask <- vapply(
        X = .data,
        FUN = function(column_data){
          is.null(dim(column_data)) && (is.numeric(column_data) || is.logical(column_data))
        },
        FUN.VALUE = logical(1L)
      )
      selected_indexes <- seq_along(numeric_mask)[numeric_mask]
    } else {
      selected_indexes <- seq_along(.data)
    }

    names(selected_indexes) <- names(.data)[selected_indexes]

    # Return selected column indexes with stable names
    return(selected_indexes)
  }

  # Evaluate user-provided selectors against data columns
  selected_indexes <- tidyselect::eval_select(rlang::expr(c(!!!selector_expressions)), .data)

  # Return explicit selection indexes for downstream slicing
  return(selected_indexes)
}
####
## End
# 
