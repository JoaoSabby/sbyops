threadCapture <- getFromNamespace("sby_internal_capture_thread_context", "sbyops")
threadApply <- getFromNamespace("sby_internal_apply_thread_context", "sbyops")
threadRestore <- getFromNamespace("sby_internal_restore_thread_context", "sbyops")
threadWith <- getFromNamespace("sby_internal_with_thread_context", "sbyops")

local_env_state <- function(key, value){
  old <- Sys.getenv(key, unset = NA_character_)
  if(is.na(value)) Sys.unsetenv(key) else do.call(Sys.setenv, setNames(list(value), key))
  on.exit({ if(is.na(old)) Sys.unsetenv(key) else do.call(Sys.setenv, setNames(list(old), key)) }, add = TRUE)
}

test_that("sby_config valida sby_config_max_threads", {
  expect_error(sby_config(sby_config_max_threads = 0), "positive integer scalar")
  expect_error(sby_config(sby_config_max_threads = c(1, 2)), "positive integer scalar")
  expect_error(sby_config(sby_config_max_threads = NA_real_), "positive integer scalar")
  expect_error(sby_config(sby_config_max_threads = Inf), "positive integer scalar")
  cfg <- sby_config(sby_config_max_threads = 3)
  expect_identical(names(cfg), c("sby_config_start_fortran", "sby_config_start_blas", "sby_config_max_threads"))
  expect_identical(cfg$sby_config_max_threads, 3L)
})

test_that("thread context restaura variaveis ausentes e vazias", {
  oldOmp <- Sys.getenv("OMP_NUM_THREADS", unset = NA_character_)
  oldMkl <- Sys.getenv("MKL_NUM_THREADS", unset = NA_character_)
  oldOpenblas <- Sys.getenv("OPENBLAS_NUM_THREADS", unset = NA_character_)
  oldBlis <- Sys.getenv("BLIS_NUM_THREADS", unset = NA_character_)
  on.exit({
    if(is.na(oldOmp)) Sys.unsetenv("OMP_NUM_THREADS") else Sys.setenv(OMP_NUM_THREADS = oldOmp)
    if(is.na(oldMkl)) Sys.unsetenv("MKL_NUM_THREADS") else Sys.setenv(MKL_NUM_THREADS = oldMkl)
    if(is.na(oldOpenblas)) Sys.unsetenv("OPENBLAS_NUM_THREADS") else Sys.setenv(OPENBLAS_NUM_THREADS = oldOpenblas)
    if(is.na(oldBlis)) Sys.unsetenv("BLIS_NUM_THREADS") else Sys.setenv(BLIS_NUM_THREADS = oldBlis)
  }, add = TRUE)

  Sys.unsetenv("OMP_NUM_THREADS")
  Sys.setenv(MKL_NUM_THREADS = "")
  ctx <- threadCapture()
  threadApply(4L, ctx)
  expect_identical(Sys.getenv("OMP_NUM_THREADS", unset = NA_character_), "4")
  threadRestore(ctx)
  expect_true(is.na(Sys.getenv("OMP_NUM_THREADS", unset = NA_character_)))
  expect_identical(Sys.getenv("MKL_NUM_THREADS", unset = NA_character_), "")
})

test_that("thread context restaura apos erro", {
  old <- Sys.getenv("OMP_NUM_THREADS", unset = NA_character_)
  expect_error(threadWith(stop("boom"), maxThreads = 2L), "boom")
  expect_identical(Sys.getenv("OMP_NUM_THREADS", unset = NA_character_), old)
})

test_that("options mc.cores e Ncpus sao restauradas", {
  old <- options(mc.cores = 7L, Ncpus = 7L)
  on.exit(options(old), add = TRUE)
  threadWith(invisible(NULL), maxThreads = 2L)
  expect_identical(getOption("mc.cores"), 7L)
  expect_identical(getOption("Ncpus"), 7L)
})

test_that("sby_select_correlation nao deixa efeitos colaterais em threads", {
  old <- Sys.getenv("OMP_NUM_THREADS", unset = NA_character_)
  df <- data.frame(a = 1:20, b = 2*(1:20), c = rnorm(20))
  sby_config(sby_config_max_threads = 2L)
  sby_select_correlation(df, threshold = 0.9)
  expect_identical(Sys.getenv("OMP_NUM_THREADS", unset = NA_character_), old)
})

test_that("sby_select_modal_frequency usa sby_config_max_threads sem falhar", {
  sby_config(sby_config_max_threads = 1L)
  df <- data.frame(a = c(1,1,1,2), b = c("x","x","y","z"))
  out <- sby_select_modal_frequency(df, threshold = 0.75)
  expect_s3_class(out, "data.frame")
})
