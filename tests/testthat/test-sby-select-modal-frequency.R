asTibble <- function(x){
  if(!requireNamespace("tibble", quietly = TRUE)){
    skip("tibble nao instalado")
  }
  getFromNamespace("as_tibble", "tibble")(x)
}

modal_reference_drop <- function(x, threshold){
  if(length(x) == 0L) return(FALSE)
  enc <- as.integer(factor(x, exclude = NULL))
  ratio <- max(tabulate(enc))/length(enc)
  ratio >= threshold
}

test_that("sby_select_modal_frequency exists", {
  expect_true(exists("sby_select_modal_frequency", mode = "function"))
})

test_that("equivalence against factor reference across supported types", {
  df <- data.frame(
    lgl = c(TRUE, TRUE, NA, FALSE, TRUE, TRUE),
    int_small = c(1L,1L,2L,1L,3L,NA),
    int_large = c(100000L,200000L,100000L,300000L,100000L,NA),
    fac = factor(c("a","a",NA,"b","a","c"), levels = c("a","b","c","z")),
    num = c(0,-0,NaN,NA,1,1),
    chr = c("x","x",NA,"y","x","z"),
    unsup = I(list(1,2,3,4,5,6))
  )
  thr <- 0.5
  out <- sby_select_modal_frequency(df, threshold = thr)
  dropped <- names(df)[vapply(df, function(col) is.null(dim(col)) && (is.logical(col)||is.integer(col)||is.factor(col)||is.numeric(col)||is.character(col)) && modal_reference_drop(col,thr), logical(1))]
  expect_false(any(dropped %in% names(out)))
  expect_true("unsup" %in% names(out))
})

test_that("threshold 0 removes all selected supported and threshold 1 only constants", {
  df <- data.frame(a = c(1,1,2), b = c("k","k","k"), c = c(TRUE,FALSE,NA))
  out0 <- sby_select_modal_frequency(df, threshold = 0)
  expect_identical(names(out0), character(0))
  out1 <- sby_select_modal_frequency(df, threshold = 1)
  expect_identical(names(out1), c("a","c"))
})

test_that("supports data.frame tibble matrix and tidyselect", {
  df <- data.frame(a=c(1,1,1,2), b=c(1,2,3,4), cc=c("x","x","x","y"))
  tb <- asTibble(df)
  m <- as.matrix(df[c("a","b")])
  expect_s3_class(sby_select_modal_frequency(tb, starts_with("c"), threshold = 0.75), "tbl_df")
  expect_true(is.matrix(sby_select_modal_frequency(m, threshold = 0.75)))
  expect_identical(names(sby_select_modal_frequency(df, b, threshold = 0.75)), names(df))
})

test_that("empty selection and zero rows/cols return unchanged", {
  df <- data.frame(a=1:3)
  expect_identical(sby_select_modal_frequency(df, starts_with("zzz"), threshold = 0.5), df)
  expect_identical(sby_select_modal_frequency(df[0,,drop=FALSE], threshold = 0.5), df[0,,drop=FALSE])
  expect_identical(sby_select_modal_frequency(df[,0,drop=FALSE], threshold = 0.5), df[,0,drop=FALSE])
})

test_that("implementation does not call encode helper in main path", {
  code <- paste(readLines(file.path("R", "sby_select_modal_frequency.R")), collapse = "\n")
  expect_false(grepl("sby_internal_encode_modal_column", code, fixed = TRUE))
})
