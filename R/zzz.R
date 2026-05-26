.onLoad <- function(libname, pkgname){

  # Register default runtime configuration values for automatic backends
  options(
    sby_config_start_fortran = getOption("sby_config_start_fortran", 10000L),
    sby_config_start_blas = getOption("sby_config_start_blas", 100000L),
    sby_config_openml_threads = getOption("sby_config_openml_threads", 2L)
  )

  # Return invisibly as required by .onLoad contract
  return(invisible(NULL))
}
####
## End
# 
