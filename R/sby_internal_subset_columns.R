#' @title Internal Helper: Subset Tabular Columns
#'
#' @usage sby_internal_subset_columns(.data, columns)
#'
#' @description Subset columns consistently across data frames, tibbles, data.tables, and matrices
#'
#' @param .data A data frame, tibble, data.table, or matrix
#'
#' @param columns Integer or character column indexes to keep
#'
#' @return A tabular object containing the requested columns
sby_internal_subset_columns <- function(.data, columns){
  if(inherits(.data, "data.table")){
    column_names <- if(is.numeric(columns)){
      names(.data)[columns]
    } else {
      columns
    }

    return(.data[, column_names, with = FALSE])
  }

  .data[, columns, drop = FALSE]
}
####
## End
# 
