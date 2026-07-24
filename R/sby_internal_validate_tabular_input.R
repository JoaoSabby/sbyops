#' @title Internal Helper: Validate Tabular Input
#'
#' @usage sby_internal_validate_tabular_input(.data, validate_column_types = FALSE)
#'
#' @description Validate supported tabular input classes for selectors
#'
#' DuckDB relations are materialized as base data frames before validation. This
#' keeps the downstream native code independent from DuckDB while allowing lazy
#' DuckDB tables to be passed to every public tabular operation.
#'
#' @param .data Candidate tabular object
#'
#' @param validate_column_types Whether to validate that the provided object only
#' contains integer, double, or logical columns
#'
#' @return The validated input object
sby_internal_validate_tabular_input <- function(.data, validate_column_types = FALSE){

  # DuckDB's relational API deliberately does not inherit from data.frame. Use
  # its as.data.frame method so collection happens through DuckDB itself and no
  # optional package needs to be attached (or imported) by sbyops.
  if(inherits(.data, "duckdb_relation")){
    .data <- tryCatch(
      as.data.frame(.data),
      error = function(error){
        stop(
          paste0("Unable to materialize the DuckDB table: ", conditionMessage(error)),
          call. = FALSE
        )
      }
    )
  }

  # Abort when input is not one of the supported in-memory table structures
  if(!(inherits(.data, "data.frame") || is.matrix(.data))){
    stop("`.data` must be a data.frame, tibble, data.table, matrix, or DuckDB table", call. = FALSE)
  }

  if(isTRUE(validate_column_types)){
    # This private package is deployed for a fixed client schema: only integer,
    # double, and logical columns are accepted by the specialized native paths. The
    # public selectors call this branch only after tidyselect has reduced the
    # input to the columns that will actually be evaluated.
    if(is.matrix(.data)){
      if(!(is.integer(.data) || is.double(.data) || is.logical(.data))){
        stop("`.data` must contain only integer, double, or logical columns", call. = FALSE)
      }
    } else {
      valid_columns <- vapply(.data, function(current_column){
        is.integer(current_column) || is.double(current_column) || is.logical(current_column)
      }, logical(1L))
      if(!all(valid_columns)){
        stop("`.data` must contain only integer, double, or logical columns", call. = FALSE)
      }
    }
  }

  # Build validated output object for explicit return visibility
  validated_data <- .data

  # Return validated tabular input object
  return(validated_data)
}
####
## End
# 
