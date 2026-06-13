thread_capture <- getFromNamespace("sby_internal_capture_thread_context", "sbyops")
thread_apply <- getFromNamespace("sby_internal_apply_thread_context", "sbyops")
thread_restore <- getFromNamespace("sby_internal_restore_thread_context", "sbyops")
thread_with <- getFromNamespace("sby_internal_with_thread_context", "sbyops")

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

test_that("thread context restaura somente variaveis permitidas", {
  old_omp_limit <- Sys.getenv("OMP_THREAD_LIMIT", unset = NA_character_)
  old_mkl <- Sys.getenv("MKL_NUM_THREADS", unset = NA_character_)
  old_omp_num <- Sys.getenv("OMP_NUM_THREADS", unset = NA_character_)
  old_openblas <- Sys.getenv("OPENBLAS_NUM_THREADS", unset = NA_character_)
  on.exit({
    if(is.na(old_omp_limit)) Sys.unsetenv("OMP_THREAD_LIMIT") else Sys.setenv(OMP_THREAD_LIMIT = old_omp_limit)
    if(is.na(old_mkl)) Sys.unsetenv("MKL_NUM_THREADS") else Sys.setenv(MKL_NUM_THREADS = old_mkl)
    if(is.na(old_omp_num)) Sys.unsetenv("OMP_NUM_THREADS") else Sys.setenv(OMP_NUM_THREADS = old_omp_num)
    if(is.na(old_openblas)) Sys.unsetenv("OPENBLAS_NUM_THREADS") else Sys.setenv(OPENBLAS_NUM_THREADS = old_openblas)
  }, add = TRUE)

  Sys.unsetenv("OMP_THREAD_LIMIT")
  Sys.setenv(MKL_NUM_THREADS = "")
  Sys.setenv(OMP_NUM_THREADS = "99")
  Sys.setenv(OPENBLAS_NUM_THREADS = "88")
  ctx <- thread_capture()
  thread_apply(4L, ctx)
  expect_identical(Sys.getenv("OMP_THREAD_LIMIT", unset = NA_character_), "4")
  expect_identical(Sys.getenv("MKL_NUM_THREADS", unset = NA_character_), "4")
  expect_identical(Sys.getenv("OMP_NUM_THREADS", unset = NA_character_), "99")
  expect_identical(Sys.getenv("OPENBLAS_NUM_THREADS", unset = NA_character_), "88")
  thread_restore(ctx)
  expect_true(is.na(Sys.getenv("OMP_THREAD_LIMIT", unset = NA_character_)))
  expected_mkl <- if(.Platform$OS.type == "windows") NA_character_ else ""
  expect_identical(Sys.getenv("MKL_NUM_THREADS", unset = NA_character_), expected_mkl)
})

test_that("thread context restaura apos erro", {
  old <- Sys.getenv("OMP_THREAD_LIMIT", unset = NA_character_)
  expect_error(thread_with(stop("boom"), max_threads = 2L), "boom")
  expect_identical(Sys.getenv("OMP_THREAD_LIMIT", unset = NA_character_), old)
})

test_that("options mc.cores e Ncpus sao restauradas", {
  old <- options(mc.cores = 7L, Ncpus = 7L)
  on.exit(options(old), add = TRUE)
  thread_with(invisible(NULL), max_threads = 2L)
  expect_identical(getOption("mc.cores"), 7L)
  expect_identical(getOption("Ncpus"), 7L)
})

test_that("sby_select_correlation nao deixa efeitos colaterais em threads", {
  old <- Sys.getenv("OMP_THREAD_LIMIT", unset = NA_character_)
  df <- data.frame(a = 1:20, b = 2*(1:20), c = rnorm(20))
  sby_config(sby_config_max_threads = 2L)
  sby_select_correlation(df, threshold = 0.9)
  expect_identical(Sys.getenv("OMP_THREAD_LIMIT", unset = NA_character_), old)
})
