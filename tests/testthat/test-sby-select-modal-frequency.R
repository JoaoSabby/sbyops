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
