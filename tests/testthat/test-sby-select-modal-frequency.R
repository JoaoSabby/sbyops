test_that("sby_select_modal_frequency exists", {
  expect_true(exists("sby_select_modal_frequency", mode = "function"))
})

test_that("sby_select_modal_frequency removes high modal-frequency columns", {
  df <- data.frame(constant = c(1, 1, 1, 1, 1), near_constant = c("x", "x", "x", "x", "y"), variable = c(1, 2, 3, 4, 5), missing_modal = c(NA, NA, NA, NA, 10))
  out <- sby_select_modal_frequency(df, threshold = 0.8)
  expect_named(out, "variable")
})

test_that("sby_select_modal_frequency supports matrix", {
  m <- cbind(a = c(1, 1, 1, 1), b = c(1, 2, 3, 4))
  out <- sby_select_modal_frequency(m, threshold = 0.75)
  expect_true(is.matrix(out))
  expect_named(out, "b")
})

test_that("sby_select_modal_frequency validates threshold", {
  df <- data.frame(x = c(1, 2, 3))
  expect_error(sby_select_modal_frequency(df), "threshold")
  expect_error(sby_select_modal_frequency(df, threshold = 1.1), "between 0 and 1")
})
