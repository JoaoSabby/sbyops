testPath <- getFromNamespace("test_path", "testthat")

test_that("sby_select_modal_frequency exists", {
  expect_true(exists("sby_select_modal_frequency", mode = "function"))
})

test_that("implementation no longer references removed modal-frequency backend", {
  code <- paste(readLines(testPath("..", "..", "R", "sby_select_modal_frequency.R")), collapse = "\n")
  expect_false(grepl("sby_internal_encode_modal_column", code, fixed = TRUE))
  expect_false(grepl("sby_internal_is_modal_supported", code, fixed = TRUE))
  expect_false(grepl("sby_modal_frequency_mask", code, fixed = TRUE))
  expect_false(grepl("sby_modal_frequency_codes_fortran", code, fixed = TRUE))
})
