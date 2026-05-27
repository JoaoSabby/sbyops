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
#' constantData <- data.frame(a = c(1, 1, 1), b = c(1, 2, 3))
#' sby_select_non_constant(constantData)
#' @export
sby_select_non_constant <- function(.data, ...){

  sby_internal_validate_tabular_input(.data = .data)

  if(fncol(.data) == 0L || fnrow(.data) == 0L){
    return(.data)
  }

  resolvedNames <- sby_internal_resolve_column_names(.data = .data)
  colnames(.data) <- resolvedNames

  selectedColumns <- sby_internal_eval_select(
    .data = .data,
    ...,
    default = "all"
  )
  if(length(selectedColumns) == 0L){
    return(.data)
  }

  selectedData <- .data[, unname(selectedColumns), drop = FALSE]
  selectedList <- as.list(as.data.frame(selectedData, stringsAsFactors = FALSE))

  keepMask <- .Call(
    "sby_non_constant_mask",
    selectedList,
    PACKAGE = "sbyops"
  )
  removedColumns <- colnames(selectedData)[!keepMask]
  keptColumns <- setdiff(colnames(.data), removedColumns)
  filteredData <- .data[, keptColumns, drop = FALSE]

  sby_internal_restore_selected_data(
    selected_data = filteredData,
    original = .data
  )
}
####
## End
# 
