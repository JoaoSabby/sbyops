testPath <- getFromNamespace("test_path", "testthat")

sby_internal_read_package_file <- function(...){
  paste(readLines(testPath("..", "..", ...)), collapse = "\n")
}

test_that("native .Call symbols use internal prefix and are registered consistently", {
  r_files <- list.files(testPath("..", "..", "R"), pattern = "\\.R$", full.names = TRUE)
  r_code <- paste(vapply(r_files, function(path){
    paste(readLines(path), collapse = "\n")
  }, character(1L)), collapse = "\n")
  init_code <- sby_internal_read_package_file("src", "init.c")

  call_symbols <- unique(unlist(regmatches(
    r_code,
    gregexpr('\\.Call\\(\\s*[`\"]([^`\"]+)[`\"]', r_code, perl = TRUE)
  )))
  call_symbols <- sub('^\\.Call\\(\\s*[`\"]', "", call_symbols)
  call_symbols <- sub('[`\"].*$', "", call_symbols)

  expect_true(length(call_symbols) > 0L)
  expect_true(all(grepl("^(_sbyops_)?sby_internal_", call_symbols)))

  for(symbol in call_symbols){
    expect_true(grepl(sprintf('"%s"', symbol), init_code, fixed = TRUE), info = symbol)
  }

  expect_true(grepl("R_useDynamicSymbols(dll, FALSE)", init_code, fixed = TRUE))
})
