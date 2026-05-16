test_that("sby_fe_filter_nzv detects modal-frequency low-variability columns", {
  df <- data.frame(
    constant = c(1, 1, 1, 1, 1),
    near_constant = c("x", "x", "x", "x", "y"),
    variable = c(1, 2, 3, 4, 5),
    missing_modal = c(NA, NA, NA, NA, 10),
    stringsAsFactors = FALSE
  )

  res <- sby_fe_filter_nzv(df, threshold = 0.8, n_threads = 1)

  expect_s3_class(res, "sby_fe_filter_nzv_result")
  expect_named(res, c("column", "ratio", "value", "count"))
  expect_equal(sort(res$column), c("constant", "missing_modal", "near_constant"))
  expect_equal(attr(res, "n_rows"), 5L)
  expect_equal(attr(res, "n_cols"), 4L)
  expect_equal(attr(res, "threshold"), 0.8)
  expect_equal(attr(res, "n_threads"), 1L)
})

test_that("threshold accepts the closed interval and rejects invalid values", {
  df <- data.frame(x = c(1, 2, 3))

  expect_s3_class(sby_fe_filter_nzv(df, threshold = 0), "sby_fe_filter_nzv_result")
  expect_equal(nrow(sby_fe_filter_nzv(df, threshold = 1)), 0L)

  expect_error(sby_fe_filter_nzv(df), "threshold")
  expect_error(sby_fe_filter_nzv(df, threshold = -0.1), "entre 0 e 1")
  expect_error(sby_fe_filter_nzv(df, threshold = 1.1), "entre 0 e 1")
  expect_error(sby_fe_filter_nzv(df, threshold = NA_real_), "não ausente")
  expect_error(sby_fe_filter_nzv(df, threshold = NaN), "não ausente")
  expect_error(sby_fe_filter_nzv(df, threshold = Inf), "finito")
  expect_error(sby_fe_filter_nzv(df, threshold = c(0.5, 0.6)), "escalar")
})

test_that("n_threads validation rejects unsafe values", {
  df <- data.frame(x = c(1, 1, 2))

  expect_s3_class(sby_fe_filter_nzv(df, threshold = 0.5, n_threads = NULL), "sby_fe_filter_nzv_result")
  expect_s3_class(sby_fe_filter_nzv(df, threshold = 0.5, n_threads = 1), "sby_fe_filter_nzv_result")

  expect_error(sby_fe_filter_nzv(df, threshold = 0.5, n_threads = 0), "inteiro positivo")
  expect_error(sby_fe_filter_nzv(df, threshold = 0.5, n_threads = -1), "inteiro positivo")
  expect_error(sby_fe_filter_nzv(df, threshold = 0.5, n_threads = 1.5), "inteiro positivo")
  expect_error(sby_fe_filter_nzv(df, threshold = 0.5, n_threads = NA_real_), "inteiro positivo")
  expect_error(sby_fe_filter_nzv(df, threshold = 0.5, n_threads = Inf), "inteiro positivo")
  expect_error(
    sby_fe_filter_nzv(df, threshold = 0.5, n_threads = .Machine$integer.max + 1),
    "grande demais"
  )
})

test_that("empty inputs return standardized empty results", {
  no_cols <- data.frame()
  expect_message(res_cols <- sby_fe_filter_nzv(no_cols, threshold = 0.5), "nenhuma coluna")
  expect_s3_class(res_cols, "sby_fe_filter_nzv_result")
  expect_equal(nrow(res_cols), 0L)
  expect_equal(attr(res_cols, "n_cols"), 0L)

  no_rows <- data.frame(x = numeric())
  expect_message(res_rows <- sby_fe_filter_nzv(no_rows, threshold = 0.5), "nenhuma linha")
  expect_s3_class(res_rows, "sby_fe_filter_nzv_result")
  expect_equal(nrow(res_rows), 0L)
  expect_equal(attr(res_rows, "n_rows"), 0L)
})

test_that("supported and unsupported column types are handled explicitly", {
  df <- data.frame(
    factor_col = factor(c("a", "a", NA)),
    character_col = c("b", "b", "c"),
    integer_col = c(1L, 1L, 2L),
    logical_col = c(TRUE, TRUE, FALSE),
    numeric_col = c(1, 1, 2)
  )

  expect_s3_class(sby_fe_filter_nzv(df, threshold = 2 / 3, n_threads = 1), "sby_fe_filter_nzv_result")

  expect_error(
    sby_fe_filter_nzv(data.frame(date_col = as.Date("2026-01-01") + 0:2), threshold = 0.5),
    "Colunas inválidas"
  )

  list_df <- data.frame(x = I(list(1, 1, 2)))
  expect_error(sby_fe_filter_nzv(list_df, threshold = 0.5), "Colunas inválidas")

  matrix_df <- data.frame(x = I(matrix(1:6, nrow = 3)))
  expect_error(sby_fe_filter_nzv(matrix_df, threshold = 0.5), "Colunas inválidas")
})

test_that("ties are deterministic and use the first factor code", {
  df <- data.frame(x = c("b", "a", "b", "a"), stringsAsFactors = FALSE)
  res <- sby_fe_filter_nzv(df, threshold = 0.5, n_threads = 1)

  expect_equal(res$column, "x")
  expect_equal(res$ratio, 0.5)
  expect_equal(res$count, 2L)
  expect_equal(res$value, "a")
})

test_that("print method returns the object invisibly", {
  res <- sby_fe_filter_nzv(data.frame(x = c(1, 1, 2)), threshold = 0.5, n_threads = 1)
  printed <- capture.output(ret <- print(res))

  expect_true(any(grepl("<sby_fe_filter_nzv_result>", printed, fixed = TRUE)))
  expect_identical(ret, res)
})
