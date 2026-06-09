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

test_that("sby_select_modal_frequency does not depend on modal-frequency native symbol lookup", {
  code <- paste(readLines(testPath("..", "..", "R", "sby_select_modal_frequency.R")), collapse = "\n")
  init_code <- paste(readLines(testPath("..", "..", "src", "init.c")), collapse = "\n")

  expect_false(grepl('"sby_internal_modal_frequency_keep_mask"', code, fixed = TRUE))
  expect_false(grepl('"sby_internal_modal_frequency_keep_mask"', init_code, fixed = TRUE))
  expect_false(file.exists(testPath("..", "..", "src", "sby_internal_modal_frequency.c")))
  expect_false(grepl(".Call", code, fixed = TRUE))
  expect_true(grepl("kit::countOccur", code, fixed = TRUE))
})

test_that("sby_select_modal_frequency removes columns at or above modal threshold", {
  modal_data <- data.frame(
    remove_me = c("a", "a", "a", "b"),
    keep_me = c("a", "b", "c", "d"),
    stringsAsFactors = FALSE
  )

  filtered_data <- sby_select_modal_frequency(
    .data = modal_data,
    threshold = 0.75
  )

  expect_identical(names(filtered_data), "keep_me")
})


test_that("sby_select_modal_frequency limits modal filtering to tidyselect columns", {
  modal_data <- data.frame(
    selected_remove = c("a", "a", "a", "b"),
    unselected_modal = c("z", "z", "z", "w"),
    keep_me = c("a", "b", "c", "d"),
    stringsAsFactors = FALSE
  )

  filtered_data <- sby_select_modal_frequency(
    .data = modal_data,
    selected_remove,
    threshold = 0.75
  )

  expect_identical(names(filtered_data), c("unselected_modal", "keep_me"))
})

test_that("sby_select_modal_frequency supports tidyselect exclusion", {
  modal_data <- data.frame(
    excluded_modal = c("a", "a", "a", "b"),
    selected_modal = c("z", "z", "z", "w"),
    keep_me = c("a", "b", "c", "d"),
    stringsAsFactors = FALSE
  )

  filtered_data <- sby_select_modal_frequency(
    .data = modal_data,
    -excluded_modal,
    threshold = 0.75
  )

  expect_identical(names(filtered_data), c("excluded_modal", "keep_me"))
})

test_that("Fortran sources use lowercase extensions only", {
  package_files <- list.files(testPath("..", ".."), recursive = TRUE, all.files = FALSE, full.names = FALSE)
  fortran_files <- package_files[grepl("\\.[Ff](90|95|03|08)?$", package_files)]

  expect_true(length(fortran_files) > 0L)
  expect_false(any(grepl("\\.(F|F90|F95|F03|F08)$", fortran_files)))
})
