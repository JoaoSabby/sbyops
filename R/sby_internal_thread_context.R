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

sby_internal_validate_max_threads <- function(maxThreads){
  if(!is.numeric(maxThreads) || length(maxThreads) != 1L || is.na(maxThreads) ||
     !is.finite(maxThreads) || maxThreads < 1){
    stop("`sby_config_max_threads` must be a positive integer scalar", call. = FALSE)
  }
  as.integer(maxThreads)
}

sby_internal_get_max_threads <- function(){
  sby_internal_validate_max_threads(getOption("sby_config_max_threads", 2L))
}

sby_internal_get_rhpcblasctl <- function(){
  if(!requireNamespace("RhpcBLASctl", quietly = TRUE)) return(NULL)
  ns <- getNamespace("RhpcBLASctl")
  list(
    blasGet = if(exists("blas_get_num_procs", envir = ns, mode = "function")) get("blas_get_num_procs", envir = ns) else NULL,
    blasSet = if(exists("blas_set_num_threads", envir = ns, mode = "function")) get("blas_set_num_threads", envir = ns) else NULL,
    ompGet = if(exists("omp_get_max_threads", envir = ns, mode = "function")) get("omp_get_max_threads", envir = ns) else NULL,
    ompSet = if(exists("omp_set_num_threads", envir = ns, mode = "function")) get("omp_set_num_threads", envir = ns) else NULL
  )
}

sby_internal_capture_thread_context <- function(useOpenmp = TRUE, useBlas = TRUE){
  envVars <- sby_internal_thread_env_vars()
  current <- Sys.getenv(envVars, unset = NA_character_)
  context <- list(
    envVars = current,
    envMissing = is.na(current),
    optionsValues = options("mc.cores", "Ncpus"),
    rhpc = NULL
  )

  rhpc <- sby_internal_get_rhpcblasctl()
  if(!is.null(rhpc)){
    rhpcState <- list(used = TRUE, blasThreads = NULL, ompThreads = NULL, canRestoreBlas = FALSE, canRestoreOmp = FALSE)
    if(useBlas && !is.null(rhpc$blasGet) && !is.null(rhpc$blasSet)){
      val <- suppressWarnings(tryCatch(as.integer(rhpc$blasGet()), error = function(e) NULL))
      if(!is.null(val) && length(val) == 1L && !is.na(val) && val >= 1L){
        rhpcState$blasThreads <- val
        rhpcState$canRestoreBlas <- TRUE
      }
    }
    if(useOpenmp && !is.null(rhpc$ompGet) && !is.null(rhpc$ompSet)){
      val <- suppressWarnings(tryCatch(as.integer(rhpc$ompGet()), error = function(e) NULL))
      if(!is.null(val) && length(val) == 1L && !is.na(val) && val >= 1L){
        rhpcState$ompThreads <- val
        rhpcState$canRestoreOmp <- TRUE
      }
    }
    context$rhpc <- c(rhpcState, rhpc)
  }

  context
}

sby_internal_apply_thread_context <- function(maxThreads, threadContext, useOpenmp = TRUE, useBlas = TRUE){
  maxThreads <- sby_internal_validate_max_threads(maxThreads)
  envUpdate <- c(
    OMP_NUM_THREADS = as.character(maxThreads),
    OMP_THREAD_LIMIT = as.character(maxThreads),
    OMP_DYNAMIC = "FALSE",
    OMP_MAX_ACTIVE_LEVELS = "1",
    MKL_NUM_THREADS = as.character(maxThreads),
    MKL_DYNAMIC = "FALSE",
    OPENBLAS_NUM_THREADS = as.character(maxThreads),
    GOTO_NUM_THREADS = as.character(maxThreads),
    BLIS_NUM_THREADS = as.character(maxThreads),
    VECLIB_MAXIMUM_THREADS = as.character(maxThreads),
    NUMEXPR_NUM_THREADS = as.character(maxThreads),
    RCPP_PARALLEL_NUM_THREADS = as.character(maxThreads)
  )

  if(!useOpenmp){
    envUpdate <- envUpdate[setdiff(names(envUpdate), c("OMP_NUM_THREADS", "OMP_THREAD_LIMIT", "OMP_DYNAMIC", "OMP_MAX_ACTIVE_LEVELS"))]
  }
  if(!useBlas){
    envUpdate <- envUpdate[setdiff(names(envUpdate), c("MKL_NUM_THREADS", "MKL_DYNAMIC", "OPENBLAS_NUM_THREADS", "GOTO_NUM_THREADS", "BLIS_NUM_THREADS", "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"))]
  }

  do.call(Sys.setenv, as.list(envUpdate))
  options(mc.cores = maxThreads, Ncpus = maxThreads)

  if(!is.null(threadContext$rhpc) && isTRUE(threadContext$rhpc$used)){
    if(useBlas && !is.null(threadContext$rhpc$blasSet)) suppressWarnings(try(threadContext$rhpc$blasSet(maxThreads), silent = TRUE))
    if(useOpenmp && !is.null(threadContext$rhpc$ompSet)) suppressWarnings(try(threadContext$rhpc$ompSet(maxThreads), silent = TRUE))
  }
}

sby_internal_restore_thread_context <- function(threadContext){
  envValues <- threadContext$envVars
  envMissing <- threadContext$envMissing
  for(i in seq_along(envValues)){
    key <- names(envValues)[i]
    if(envMissing[[i]]) Sys.unsetenv(key) else do.call(Sys.setenv, setNames(list(envValues[[i]]), key))
  }

  oldMc <- threadContext$optionsValues$mc.cores
  oldNcpus <- threadContext$optionsValues$Ncpus
  if(is.null(oldMc)) options(mc.cores = NULL) else options(mc.cores = oldMc)
  if(is.null(oldNcpus)) options(Ncpus = NULL) else options(Ncpus = oldNcpus)

  if(!is.null(threadContext$rhpc) && isTRUE(threadContext$rhpc$used)){
    if(isTRUE(threadContext$rhpc$canRestoreBlas) && !is.null(threadContext$rhpc$blasSet)) suppressWarnings(try(threadContext$rhpc$blasSet(threadContext$rhpc$blasThreads), silent = TRUE))
    if(isTRUE(threadContext$rhpc$canRestoreOmp) && !is.null(threadContext$rhpc$ompSet)) suppressWarnings(try(threadContext$rhpc$ompSet(threadContext$rhpc$ompThreads), silent = TRUE))
  }
}

sby_internal_with_thread_context <- function(expr, maxThreads = NULL, useOpenmp = TRUE, useBlas = TRUE){
  if(is.null(maxThreads)) maxThreads <- sby_internal_get_max_threads()
  context <- sby_internal_capture_thread_context(useOpenmp = useOpenmp, useBlas = useBlas)
  on.exit(sby_internal_restore_thread_context(context), add = TRUE)
  sby_internal_apply_thread_context(maxThreads = maxThreads, threadContext = context, useOpenmp = useOpenmp, useBlas = useBlas)
  force(expr)
}
