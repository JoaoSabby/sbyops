sby_internal_thread_env_vars <- function(){
  c(
    "OMP_NUM_THREADS",
    "OMP_THREAD_LIMIT",
    "OMP_DYNAMIC",
    "OMP_PROC_BIND",
    "OMP_PLACES",
    "OMP_MAX_ACTIVE_LEVELS",
    "MKL_NUM_THREADS",
    "MKL_DYNAMIC",
    "MKL_DOMAIN_NUM_THREADS",
    "OPENBLAS_NUM_THREADS",
    "GOTO_NUM_THREADS",
    "BLIS_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS",
    "NUMEXPR_NUM_THREADS",
    "RCPP_PARALLEL_NUM_THREADS"
  )
}

sby_internal_validate_max_threads <- function(max_threads){
  if(!is.numeric(max_threads) || length(max_threads) != 1L || is.na(max_threads) ||
     !is.finite(max_threads) || max_threads < 1){
    stop("`sby_config_max_threads` must be a positive integer scalar", call. = FALSE)
  }
  as.integer(max_threads)
}

sby_internal_get_max_threads <- function(){
  sby_internal_validate_max_threads(getOption("sby_config_max_threads", 2L))
}

sby_internal_get_rhpcblasctl <- function(){
  if(!requireNamespace("RhpcBLASctl", quietly = TRUE)) return(NULL)
  ns <- getNamespace("RhpcBLASctl")
  list(
    blas_get = if(exists("blas_get_num_procs", envir = ns, mode = "function")) get("blas_get_num_procs", envir = ns) else NULL,
    blas_set = if(exists("blas_set_num_threads", envir = ns, mode = "function")) get("blas_set_num_threads", envir = ns) else NULL,
    omp_get = if(exists("omp_get_max_threads", envir = ns, mode = "function")) get("omp_get_max_threads", envir = ns) else NULL,
    omp_set = if(exists("omp_set_num_threads", envir = ns, mode = "function")) get("omp_set_num_threads", envir = ns) else NULL
  )
}

sby_internal_capture_thread_context <- function(use_openmp = TRUE, use_blas = TRUE){
  env_vars <- sby_internal_thread_env_vars()
  current <- Sys.getenv(env_vars, unset = NA_character_)
  context <- list(
    env_vars = current,
    env_missing = is.na(current),
    options_values = options("mc.cores", "Ncpus"),
    rhpc = NULL
  )

  rhpc <- sby_internal_get_rhpcblasctl()
  if(!is.null(rhpc)){
    rhpc_state <- list(used = TRUE, blas_threads = NULL, omp_threads = NULL, can_restore_blas = FALSE, can_restore_omp = FALSE)
    if(use_blas && !is.null(rhpc$blas_get) && !is.null(rhpc$blas_set)){
      val <- suppressWarnings(tryCatch(as.integer(rhpc$blas_get()), error = function(e) NULL))
      if(!is.null(val) && length(val) == 1L && !is.na(val) && val >= 1L){
        rhpc_state$blas_threads <- val
        rhpc_state$can_restore_blas <- TRUE
      }
    }
    if(use_openmp && !is.null(rhpc$omp_get) && !is.null(rhpc$omp_set)){
      val <- suppressWarnings(tryCatch(as.integer(rhpc$omp_get()), error = function(e) NULL))
      if(!is.null(val) && length(val) == 1L && !is.na(val) && val >= 1L){
        rhpc_state$omp_threads <- val
        rhpc_state$can_restore_omp <- TRUE
      }
    }
    context$rhpc <- c(rhpc_state, rhpc)
  }

  context
}

sby_internal_apply_thread_context <- function(max_threads, thread_context, use_openmp = TRUE, use_blas = TRUE){
  max_threads <- sby_internal_validate_max_threads(max_threads)
  env_update <- c(
    OMP_NUM_THREADS = as.character(max_threads),
    OMP_THREAD_LIMIT = as.character(max_threads),
    OMP_DYNAMIC = "FALSE",
    OMP_MAX_ACTIVE_LEVELS = "1",
    MKL_NUM_THREADS = as.character(max_threads),
    MKL_DYNAMIC = "FALSE",
    OPENBLAS_NUM_THREADS = as.character(max_threads),
    GOTO_NUM_THREADS = as.character(max_threads),
    BLIS_NUM_THREADS = as.character(max_threads),
    VECLIB_MAXIMUM_THREADS = as.character(max_threads),
    NUMEXPR_NUM_THREADS = as.character(max_threads),
    RCPP_PARALLEL_NUM_THREADS = as.character(max_threads)
  )

  if(!use_openmp){
    env_update <- env_update[setdiff(names(env_update), c("OMP_NUM_THREADS", "OMP_THREAD_LIMIT", "OMP_DYNAMIC", "OMP_MAX_ACTIVE_LEVELS"))]
  }
  if(!use_blas){
    env_update <- env_update[setdiff(names(env_update), c("MKL_NUM_THREADS", "MKL_DYNAMIC", "OPENBLAS_NUM_THREADS", "GOTO_NUM_THREADS", "BLIS_NUM_THREADS", "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"))]
  }

  do.call(Sys.setenv, as.list(env_update))
  options(mc.cores = max_threads, Ncpus = max_threads)

  if(!is.null(thread_context$rhpc) && isTRUE(thread_context$rhpc$used)){
    if(use_blas && !is.null(thread_context$rhpc$blas_set)) suppressWarnings(try(thread_context$rhpc$blas_set(max_threads), silent = TRUE))
    if(use_openmp && !is.null(thread_context$rhpc$omp_set)) suppressWarnings(try(thread_context$rhpc$omp_set(max_threads), silent = TRUE))
  }
}

sby_internal_restore_thread_context <- function(thread_context){
  env_values <- thread_context$env_vars
  env_missing <- thread_context$env_missing
  for(i in seq_along(env_values)){
    key <- names(env_values)[i]
    if(env_missing[[i]]) Sys.unsetenv(key) else do.call(Sys.setenv, setNames(list(env_values[[i]]), key))
  }

  old_mc <- thread_context$options_values$mc.cores
  old_ncpus <- thread_context$options_values$Ncpus
  if(is.null(old_mc)) options(mc.cores = NULL) else options(mc.cores = old_mc)
  if(is.null(old_ncpus)) options(Ncpus = NULL) else options(Ncpus = old_ncpus)

  if(!is.null(thread_context$rhpc) && isTRUE(thread_context$rhpc$used)){
    if(isTRUE(thread_context$rhpc$can_restore_blas) && !is.null(thread_context$rhpc$blas_set)) suppressWarnings(try(thread_context$rhpc$blas_set(thread_context$rhpc$blas_threads), silent = TRUE))
    if(isTRUE(thread_context$rhpc$can_restore_omp) && !is.null(thread_context$rhpc$omp_set)) suppressWarnings(try(thread_context$rhpc$omp_set(thread_context$rhpc$omp_threads), silent = TRUE))
  }
}

sby_internal_with_thread_context <- function(expr, max_threads = NULL, use_openmp = TRUE, use_blas = TRUE){
  if(is.null(max_threads)) max_threads <- sby_internal_get_max_threads()
  context <- sby_internal_capture_thread_context(use_openmp = use_openmp, use_blas = use_blas)
  on.exit(sby_internal_restore_thread_context(context), add = TRUE)
  sby_internal_apply_thread_context(max_threads = max_threads, thread_context = context, use_openmp = use_openmp, use_blas = use_blas)
  force(expr)
}
