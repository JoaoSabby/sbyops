#' @title Internal Helper: Validate Tabular Input
#'
#' @usage sby_internal_validate_tabular_input(.data, validate_column_types = FALSE)
#'
#' @description Validate supported tabular input classes for selectors
#'
#' @param .data Candidate tabular object
#'
#' @param validate_column_types Whether to validate that the provided object only
#' contains integer, double, or logical columns
#'
#' @return The validated input object
sby_internal_validate_tabular_input <- function(.data, validate_column_types = FALSE){

  # Abort when input is not a data frame, tibble, or matrix
  if(!(inherits(.data, "data.frame") || is.matrix(.data))){
    stop("`.data` must be a data.frame, tibble, or matrix", call. = FALSE)
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
