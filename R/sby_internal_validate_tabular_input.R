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

  # Build validated output object for explicit return visibility
  validated_data <- .data

  # Return validated tabular input object
  return(validated_data)
}
####
## End
# 
