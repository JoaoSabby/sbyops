#' @title Internal Helper: Validate Tabular Input
#'
#' @usage sby_internal_validate_tabular_input(.data)
#'
#' @description Validate supported tabular input classes for selectors
#'
#' @param .data Candidate tabular object
#'
#' @return The validated input object
sby_internal_validate_tabular_input <- function(.data){

  # Abort when input is not a data frame, tibble, or matrix
  if(!(inherits(.data, "data.frame") || is.matrix(.data))){
    stop("`.data` must be a data.frame, tibble, or matrix", call. = FALSE)
  }

  # This private package is deployed for a fixed client schema: only integer
  # and double columns are accepted. Other column types are rejected early so
  # the native code can stay specialized for the server workload.
  if(is.matrix(.data)){
    if(!(is.integer(.data) || is.double(.data))){
      stop("`.data` must contain only integer or double columns", call. = FALSE)
    }
  } else {
    valid_columns <- vapply(.data, function(current_column){
      is.integer(current_column) || is.double(current_column)
    }, logical(1L))
    if(!all(valid_columns)){
      stop("`.data` must contain only integer or double columns", call. = FALSE)
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
