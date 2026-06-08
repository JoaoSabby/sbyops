asTibble <- function(x){
  if(!requireNamespace("tibble", quietly = TRUE)){
    skip("tibble nao instalado")
  }
  getFromNamespace("as_tibble", "tibble")(x)
}

# Este arquivo cobre contratos internos de correlacao, estrategia e restauracao estrutural.

computeStreaming <- getFromNamespace("sby_internal_compute_correlation_streaming", "sbyops")
computeBlas <- getFromNamespace("sby_internal_compute_correlation_blas", "sbyops")
computeFortran <- getFromNamespace("sby_internal_compute_correlation_fortran", "sbyops")
applySelection <- getFromNamespace("sby_internal_apply_correlation_selection", "sbyops")
estimateMemory <- getFromNamespace("sby_internal_estimate_correlation_memory", "sbyops")
inspectProfile <- getFromNamespace("sby_internal_inspect_matrix_profile", "sbyops")
selectStrategy <- getFromNamespace("sby_internal_select_correlation_strategy", "sbyops")
restoreSelected <- getFromNamespace("sby_internal_restore_selected_data", "sbyops")


test_that("helpers de correlacao retornam matriz quadrada com diagonal zerada", {
  m <- cbind(a = 1:10, b = 2 * (1:10), c = rnorm(10))
  cs <- computeStreaming(m)
  cb <- computeBlas(m)
  cf <- computeFortran(m, threshold = 0.99)
  expect_equal(dim(cs), c(3L, 3L))
  expect_equal(unname(diag(cs)), c(0, 0, 0))
  expect_equal(unname(diag(cb)), c(0, 0, 0))
  expect_type(cf, "character")
})

test_that("streaming e blas sao numericamente proximos em matriz pequena", {
  set.seed(123)
  m <- matrix(rnorm(200), ncol = 4)
  expect_equal(computeStreaming(m), computeBlas(m), tolerance = 1e-10)
})

test_that("fortran e R base sao equivalentes com NA pairwise", {
  m <- cbind(a = c(1, 2, NA, 4, 5), b = c(2, 4, NA, 8, 10), c = c(1, 0, 1, 0, 1))
  cor_r <- abs(cor(m, use = "pairwise.complete.obs"))
  expect_equal(
    computeFortran(m, threshold = 0.99),
    applySelection(cor_r, threshold = 0.99)
  )
})

test_that("sby_internal_apply_correlation_selection remove colunas por limiar inclusivo", {
  cm <- matrix(c(0, 0.99, 0.1,
                 0.99, 0, 0.2,
                 0.1, 0.2, 0), ncol = 3, byrow = TRUE)
  colnames(cm) <- c("a", "b", "c")
  rem <- applySelection(cm, threshold = 0.99)
  expect_true(length(rem) == 1L)
  expect_true(rem %in% c("a", "b"))
})

test_that("sby_internal_apply_correlation_selection respeita limiar alto sem remocoes", {
  cm <- diag(3)
  colnames(cm) <- c("x", "y", "z")
  expect_identical(applySelection(cm, threshold = 1), character(0))
})

test_that("sby_internal_estimate_correlation_memory cresce com dimensoes", {
  small <- estimateMemory(10, 2)
  big <- estimateMemory(100, 10)
  expect_true(small$matrix_bytes > 0)
  expect_true(big$matrix_bytes > small$matrix_bytes)
  expect_true(big$corr_bytes > small$corr_bytes)
})

test_that("sby_internal_inspect_matrix_profile retorna campos esperados", {
  m <- matrix(c(1, 2, NA, 4), ncol = 2)
  p <- inspectProfile(m)
  expect_true(all(c("n_rows", "n_cols", "n_pairs", "has_non_finite", "matrix_bytes", "corr_bytes") %in% names(p)))
  expect_identical(p$n_cols, 2L)
  expect_true(p$has_non_finite)
})

test_that("sby_internal_select_correlation_strategy respeita thresholds em fronteiras", {
  old <- options(sby_config_start_fortran = 10L, sby_config_start_blas = 20L)
  on.exit(options(old), add = TRUE)
  expect_identical(selectStrategy(matrix(1, nrow = 2, ncol = 4)), "streaming")
  expect_identical(selectStrategy(matrix(1, nrow = 3, ncol = 4)), "fortran")
  expect_identical(selectStrategy(matrix(1, nrow = 5, ncol = 4)), "blas")
})

test_that("sby_internal_restore_selected_data preserva classe e estrutura", {
  df <- data.frame(a = 1:3, b = 3:1)
  expect_s3_class(restoreSelected(df[, "a", drop = FALSE], df), "data.frame")

  tb <- asTibble(df)
  outTb <- restoreSelected(tb[, "a", drop = FALSE], tb)
  expect_s3_class(outTb, "tbl_df")

  m <- as.matrix(df)
  outM <- restoreSelected(m[, 1, drop = FALSE], m)
  expect_true(is.matrix(outM))
})
