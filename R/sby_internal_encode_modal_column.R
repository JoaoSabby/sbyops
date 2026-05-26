#' @title Internal Helper: Encode Modal Column
#'
#' @usage sby_internal_encode_modal_column(x)
#'
#' @description Encode a supported vector into integer modal codes while preserving missing-value identity
#'
#' @details This helper converts input values to a factor representation with `exclude = NULL` to keep missing values as explicit levels, then returns integer codes and maximum code cardinality for native modal-frequency routines
#'
#' @section Internal contract:
#' This function is internal and should receive a one-dimensional supported vector previously validated by upstream selectors
#'
#' @param x A one-dimensional atomic vector supported by modal-frequency selection
#'
#' @return A list with integer `codes` and integer scalar `max_code`
#'
#' @seealso [sby_select_modal_frequency()]
#'
#' @references R CORE TEAM. Writing R Extensions. Vienna: R Foundation. Available at: https://cran.r-project.org/doc/manuals/r-release/R-exts.html
#'
#' @examples
#' exampleVector <- c("a", "a", NA, "b")
#' sby_internal_encode_modal_column(exampleVector)
sby_internal_encode_modal_column <- function(x){

  # Build a factor that keeps missing values as explicit levels for stable coding
  encodedFactor <- factor(x, exclude = NULL)

  # Convert factor levels to integer codes expected by the native backend
  encodedCodes <- as.integer(encodedFactor)

  # Compute maximum code index to inform native memory allocation bounds
  maximumCode <- nlevels(encodedFactor)

  # Pack encoded outputs in a deterministic internal list structure
  encodedResult <- list(codes = encodedCodes, max_code = maximumCode)

  # Return encoded payload for downstream native modal-frequency computation
  return(encodedResult)
}
####
## End
# 
