test_that("sby_select_correlation removes highly correlated numeric columns", {
  df <- data.frame(
    x1 = 1:6,
    x2 = 2 * (1:6),
    x3 = c(6, 1, 5, 2, 4, 3),
    group = letters[1:6],
    stringsAsFactors = FALSE
  )

  out <- sby_select_correlation(df, threshold = 0.99)

  expect_equal(length(intersect(names(out), c("x1", "x2"))), 1L)
  expect_true("x3" %in% names(out))
  expect_true("group" %in% names(out))
})

test_that("sby_select_correlation respects tidyselect selections", {
  df <- data.frame(
    id = letters[1:6],
    x1 = 1:6,
    x2 = 2 * (1:6),
    x3 = c(6, 1, 5, 2, 4, 3),
    stringsAsFactors = FALSE
  )

  out <- sby_select_correlation(df, x1, x2, threshold = 0.99)

  expect_equal(length(intersect(names(out), c("x1", "x2"))), 1L)
  expect_true("x3" %in% names(out))
  expect_true("id" %in% names(out))
})

test_that("sby_select_correlation ignores non-numeric selected columns", {
  df <- data.frame(x = 1:3, y = letters[1:3], stringsAsFactors = FALSE)

  out <- sby_select_correlation(df, x, y, threshold = 0.9)

  expect_named(out, c("x", "y"))
})

test_that("sby_select_correlation handles missing values pairwise", {
  df <- data.frame(
    x1 = c(1, 2, NA, 4, 5),
    x2 = c(2, 4, NA, 8, 10),
    x3 = c(5, 1, 3, 2, 4)
  )

  out <- sby_select_correlation(df, threshold = 0.99)

  expect_equal(length(intersect(names(out), c("x1", "x2"))), 1L)
  expect_true("x3" %in% names(out))
})

test_that("sby_select_correlation is stable across OpenMP thread counts", {
  old_threads <- Sys.getenv("OMP_NUM_THREADS", unset = NA_character_)
  on.exit({
    if (is.na(old_threads)) Sys.unsetenv("OMP_NUM_THREADS") else Sys.setenv(OMP_NUM_THREADS = old_threads)
  }, add = TRUE)

  set.seed(123)
  df <- data.frame(
    x1 = rnorm(500),
    x2 = rnorm(500),
    x3 = rnorm(500),
    x4 = rnorm(500)
  )
  df$x2 <- df$x1 * 0.98 + df$x2 * 0.02

  Sys.setenv(OMP_NUM_THREADS = "1")
  out_1 <- sby_select_correlation(df, threshold = 0.95)
  Sys.setenv(OMP_NUM_THREADS = "2")
  out_2 <- sby_select_correlation(df, threshold = 0.95)

  expect_identical(out_1, out_2)
})
