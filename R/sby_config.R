#' @title Configure sbyops Execution Thresholds
#' @name sby_config
#'
#' @usage
#' sby_config(
#'   sby_config_start_fortran = 10000L,
#'   sby_config_start_blas = 100000L,
#'   sby_config_max_threads = 2L
#' )
#'
#' @description
#' Configure backend thresholds and thread settings used by automatic
#' correlation execution planning
#'
#' @details
#' This function stores runtime options consumed by internal dispatch
#' logic. Values are validated as positive integer scalars.
#'
#' Default values are:
#' - `sby_config_start_fortran = 10000L`
#' - `sby_config_start_blas = 100000L`
#' - `sby_config_max_threads = 2L`
#'
#' @param sby_config_start_fortran Integer threshold where automatic
#' execution switches from streaming to Fortran backend
#'
#' @param sby_config_start_blas Integer threshold where BLAS with
#' OpenMP threading is enabled
#'
#' @param sby_config_max_threads Integer positive scalar thread cap used by native backends
#'
#' @return A named list with validated configuration values
#'
#' @examples
#' # Apply default configuration values explicitly
#' sby_config(
#'   sby_config_start_fortran = 10000L,
#'   sby_config_start_blas = 100000L,
#'   sby_config_max_threads = 2L
#' )
#' @export
sby_config <- function(sby_config_start_fortran = 10000L,
                       sby_config_start_blas = 100000L,
                       sby_config_max_threads = 2L){

  # Validate Fortran threshold as positive integer scalar
  if(!is.numeric(sby_config_start_fortran) || length(sby_config_start_fortran) != 1L ||
     !is.finite(sby_config_start_fortran) || sby_config_start_fortran < 1L){
    stop("`sby_config_start_fortran` must be a positive integer scalar", call. = FALSE)
  }

  # Validate BLAS threshold as positive integer scalar
  if(!is.numeric(sby_config_start_blas) || length(sby_config_start_blas) != 1L ||
     !is.finite(sby_config_start_blas) || sby_config_start_blas < 1L){
    stop("`sby_config_start_blas` must be a positive integer scalar", call. = FALSE)
  }


  if(!is.numeric(sby_config_max_threads) || length(sby_config_max_threads) != 1L ||
     !is.finite(sby_config_max_threads) || sby_config_max_threads < 1L){
    stop("`sby_config_max_threads` must be a positive integer scalar", call. = FALSE)
  }

  options(
    sby_config_start_fortran = as.integer(sby_config_start_fortran),
    sby_config_start_blas = as.integer(sby_config_start_blas),
    sby_config_max_threads = as.integer(sby_config_max_threads),
    sby_config_openml_threads = as.integer(sby_config_max_threads)
  )

  # Build configuration payload for return visibility
  configuration <- list(
    sby_config_start_fortran = getOption("sby_config_start_fortran"),
    sby_config_start_blas = getOption("sby_config_start_blas"),
    sby_config_max_threads = getOption("sby_config_max_threads"),
    sby_config_openml_threads = getOption("sby_config_openml_threads")
  )

  # Return current sbyops configuration values
  return(configuration)
}
####
## End
# 
