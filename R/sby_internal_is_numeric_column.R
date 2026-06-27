#' @title Internal Helper: Is Numeric Column
#'
#' @usage sby_internal_is_numeric_column(...)
#'
#' @description Internal utility routine used by exported selectors to validate inputs, transform data representations, and coordinate native backend calls
#'
#' @details This helper is part of the package internal architecture and is not exported. The function encapsulates a single responsibility to improve maintainability, readability, and testability of the selector pipeline
#'
#' @section Internal contract:
#' This function is designed for package-internal orchestration. Inputs are assumed to be provided by validated upstream code paths unless otherwise documented in argument checks
#' @param ... Function-specific arguments consumed by this internal helper
#'
#' @return A function-specific internal object used by downstream routines in the selection workflow
#'
#' @seealso [sby_select_correlation()], [sby_select_modal_frequency()], [sby_select_non_constant()]
#'
#' @references R CORE TEAM. Writing R Extensions. Vienna: R Foundation. Available at: https://cran.r-project.org/doc/manuals/r-release/R-exts.html
#'
#' @examples
#' # Internal helper example for maintainers
#' # Not intended for direct end-user invocation
sby_internal_is_numeric_column <- function(x){
  is.null(dim(x)) && (is.numeric(x) || is.logical(x))
}
####
## End
# 
