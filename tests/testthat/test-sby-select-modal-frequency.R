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

test_that("sby_select_modal_frequency uses the registered internal native symbol", {
  code <- paste(readLines(testPath("..", "..", "R", "sby_select_modal_frequency.R")), collapse = "\n")
  init_code <- paste(readLines(testPath("..", "..", "src", "init.c")), collapse = "\n")

  expect_true(grepl('"sby_internal_modal_frequency_keep_mask"', code, fixed = TRUE))
  expect_true(grepl('"sby_internal_modal_frequency_keep_mask"', init_code, fixed = TRUE))
  expect_true(grepl('PACKAGE = "sbyops"', code, fixed = TRUE))
  expect_false(grepl("getDLLRegisteredRoutines", code, fixed = TRUE))
})

test_that("Fortran sources use lowercase extensions only", {
  package_files <- list.files(testPath("..", ".."), recursive = TRUE, all.files = FALSE, full.names = FALSE)
  fortran_files <- package_files[grepl("\\.[Ff](90|95|03|08)?$", package_files)]

  expect_true(length(fortran_files) > 0L)
  expect_false(any(grepl("\\.(F|F90|F95|F03|F08)$", fortran_files)))
})
