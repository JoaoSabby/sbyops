#' @title Remove Constant Columns
#' @name sby_select_non_constant
#'
#' @usage sby_select_non_constant(.data, ...)
#'
#' @description Remove selected columns that contain a single repeated value across all observations
#'
#' @details The native C backend evaluates each selected column and supports column-parallel execution when OpenMP is available
#'
#' @param .data A data frame, tibble, or matrix
#'
#' @param ... Tidyselect expressions for data-frame-like inputs. When omitted, all columns are considered
#'
#' @return An object with the same structural class as `.data` without selected constant columns
#'
#' @seealso [sby_select_correlation()], [sby_select_modal_frequency()]
#'
#' @examples
#' constant_data <- data.frame(a = c(1, 1, 1), b = c(1, 2, 3))
#' sby_select_non_constant(constant_data)
#' @export
sby_select_non_constant <- function(.data, ...){

  sby_internal_validate_tabular_input(.data = .data)

  if(fncol(.data) == 0L || fnrow(.data) == 0L){
    return(.data)
  }

  resolved_names <- sby_internal_resolve_column_names(.data = .data)
  colnames(.data) <- resolved_names

  selected_columns <- sby_internal_eval_select(
    .data = .data,
    ...,
    default = "all"
  )
  if(length(selected_columns) == 0L){
    return(.data)
  }

  selected_data <- .data[, unname(selected_columns), drop = FALSE]
  selected_list <- as.list(as.data.frame(selected_data, stringsAsFactors = FALSE))

  keep_mask <- .Call(
    "sby_internal_non_constant_mask",
    selected_list,
    PACKAGE = "sbyops"
  )
  removed_columns <- colnames(selected_data)[!keep_mask]
  kept_columns <- setdiff(colnames(.data), removed_columns)
  filtered_data <- .data[, kept_columns, drop = FALSE]

  sby_internal_restore_selected_data(
    selected_data = filtered_data,
    original = .data
  )
}
####
## End
# 
