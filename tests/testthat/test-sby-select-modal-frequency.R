test_that("sby_select_modal_frequency removes high modal-frequency columns", {
  df <- data.frame(
    constant = c(1, 1, 1, 1, 1),
    near_constant = c("x", "x", "x", "x", "y"),
    variable = c(1, 2, 3, 4, 5),
    missing_modal = c(NA, NA, NA, NA, 10),
    stringsAsFactors = FALSE
  )

  out <- sby_select_modal_frequency(df, threshold = 0.8)

  expect_named(out, "variable")
})

test_that("sby_select_modal_frequency respects tidyselect selections", {
  df <- data.frame(
    id = c("a", "b", "c", "d", "e"),
    constant = c(1, 1, 1, 1, 1),
    variable = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  out <- sby_select_modal_frequency(df, constant, threshold = 1)

  expect_named(out, c("id", "variable"))
})

test_that("sby_select_modal_frequency validates threshold", {
  df <- data.frame(x = c(1, 2, 3))

  expect_error(sby_select_modal_frequency(df), "threshold")
  expect_error(sby_select_modal_frequency(df, threshold = -0.1), "between 0 and 1")
  expect_error(sby_select_modal_frequency(df, threshold = 1.1), "between 0 and 1")
  expect_error(sby_select_modal_frequency(df, threshold = NA_real_), "non-missing")
})

test_that("sby_select_modal_frequency ignores unsupported selected columns", {
  df <- data.frame(
    date_col = as.Date("2026-01-01") + 0:2,
    constant = c(1, 1, 1)
  )

  out <- sby_select_modal_frequency(df, date_col, threshold = 1)
  expect_named(out, c("date_col", "constant"))
})

test_that("sby_select_modal_frequency is deterministic across OpenMP thread counts", {
  old_threads <- Sys.getenv("OMP_NUM_THREADS", unset = NA_character_)
  on.exit({
    if (is.na(old_threads)) Sys.unsetenv("OMP_NUM_THREADS") else Sys.setenv(OMP_NUM_THREADS = old_threads)
  }, add = TRUE)

  df <- data.frame(
    a = rep(1:4, each = 100),
    b = c(rep("x", 390), rep("y", 10)),
    c = 1:400
  )

  Sys.setenv(OMP_NUM_THREADS = "1")
  out_1 <- sby_select_modal_frequency(df, threshold = 0.95)
  Sys.setenv(OMP_NUM_THREADS = "2")
  out_2 <- sby_select_modal_frequency(df, threshold = 0.95)

  expect_identical(out_1, out_2)
})
