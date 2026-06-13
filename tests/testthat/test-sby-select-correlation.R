test_that("sby_select_correlation exists", {
  expect_true(exists("sby_select_correlation", mode = "function"))
})

test_that("sby_select_correlation removes highly correlated numeric columns", {
  df <- data.frame(x1 = 1:6, x2 = 2 * (1:6), x3 = c(6, 1, 5, 2, 4, 3))
  out <- sby_select_correlation(df, threshold = 0.99)
  expect_equal(length(intersect(names(out), c("x1", "x2"))), 1L)
  expect_true("x3" %in% names(out))
})

test_that("sby_select_correlation supports matrix with and without colnames", {
  m1 <- cbind(a = 1:10, b = 2 * (1:10), c = rnorm(10))
  out1 <- sby_select_correlation(m1, threshold = 0.99)
  expect_true(is.matrix(out1))
  expect_true(length(intersect(colnames(out1), c("a", "b"))) == 1L)

  m2 <- unname(m1)
  out2 <- sby_select_correlation(m2, threshold = 0.99)
  expect_true(is.matrix(out2))
  expect_false(is.null(colnames(out2)))
})

test_that("sby_select_correlation handles missing values pairwise", {
  df <- data.frame(x1 = c(1, 2, NA, 4, 5), x2 = c(2, 4, NA, 8, 10), x3 = c(5, 1, 3, 2, 4))
  out <- sby_select_correlation(df, threshold = 0.99)
  expect_equal(length(intersect(names(out), c("x1", "x2"))), 1L)
})

test_that("sby_select_correlation validates threshold", {
  df <- data.frame(x = 1:3)
  expect_error(sby_select_correlation(df), "threshold")
  expect_error(sby_select_correlation(df, threshold = -0.1), "between 0 and 1")
})

test_that("sby_select_correlation validates column types only after tidyselect", {
  df <- data.frame(
    TARGET = c("a", "b", "c", "d", "e", "f"),
    colunasId = paste0("id", 1:6),
    x1 = 1:6,
    x2 = 2 * (1:6),
    x3 = c(6, 1, 5, 2, 4, 3)
  )

  out <- sby_select_correlation(df, -TARGET, -colunasId, threshold = 0.99)
  expect_true(all(c("TARGET", "colunasId") %in% names(out)))
  expect_equal(length(intersect(names(out), c("x1", "x2"))), 1L)

  expect_error(
    sby_select_correlation(df, TARGET, threshold = 0.99),
    "`.data` must contain only integer or double columns",
    fixed = TRUE
  )
})

test_that("sby_select_correlation is stable across OpenMP thread counts", {
  skip_on_cran()
  old_threads <- Sys.getenv("OMP_NUM_THREADS", unset = NA_character_)
  on.exit({ if (is.na(old_threads)) Sys.unsetenv("OMP_NUM_THREADS") else Sys.setenv(OMP_NUM_THREADS = old_threads) }, add = TRUE)

  set.seed(123)
  df <- data.frame(x1 = rnorm(1000), x2 = rnorm(1000), x3 = rnorm(1000), x4 = rnorm(1000))
  df$x2 <- df$x1 * 0.98 + df$x2 * 0.02

  Sys.setenv(OMP_NUM_THREADS = "1", OMP_DYNAMIC = "FALSE")
  out_1 <- sby_select_correlation(df, threshold = 0.95)
  Sys.setenv(OMP_NUM_THREADS = "2", OMP_DYNAMIC = "FALSE")
  out_2 <- sby_select_correlation(df, threshold = 0.95)
  expect_identical(out_1, out_2)
})
