# Este arquivo cobre helpers internos de validacao, selecao e suporte de tipos.

validate_tabular <- getFromNamespace("sby_internal_validate_tabular_input", "sbyops")
resolve_names <- getFromNamespace("sby_internal_resolve_column_names", "sbyops")
eval_select <- getFromNamespace("sby_internal_eval_select", "sbyops")
is_numeric_column <- getFromNamespace("sby_internal_is_numeric_column", "sbyops")
validate_threshold <- getFromNamespace("sby_internal_validate_threshold_scalar", "sbyops")
validate_corr_threshold <- getFromNamespace("sby_internal_validate_correlation_threshold", "sbyops")


test_that("sby_internal_validate_tabular_input aceita classes tabulares suportadas", {
  expect_identical(validate_tabular(data.frame(a = 1)), data.frame(a = 1))
  expect_identical(validate_tabular(matrix(1:4, ncol = 2)), matrix(1:4, ncol = 2))
})

test_that("sby_internal_validate_tabular_input rejeita objetos nao tabulares", {
  expect_error(validate_tabular(1:3), "must be a data.frame")
  expect_error(validate_tabular(list(a = 1)), "must be a data.frame")
  expect_error(validate_tabular(NULL), "must be a data.frame")
})

test_that("sby_internal_validate_tabular_input aceita logical e rejeita colunas fora do contrato", {
  expect_identical(
    validate_tabular(data.frame(a = c(TRUE, FALSE)), validate_column_types = TRUE),
    data.frame(a = c(TRUE, FALSE))
  )
  expect_identical(
    validate_tabular(matrix(TRUE, ncol = 1), validate_column_types = TRUE),
    matrix(TRUE, ncol = 1)
  )
  expect_error(
    validate_tabular(data.frame(a = 1:3, b = letters[1:3]), validate_column_types = TRUE),
    "integer, double, or logical"
  )
  expect_error(
    validate_tabular(matrix("x", ncol = 1), validate_column_types = TRUE),
    "integer, double, or logical"
  )
})

test_that("sby_internal_resolve_column_names repara nomes vazios e duplicados", {
  x <- data.frame(" " = 1, a = 2, a = 3, check.names = FALSE)
  nm <- resolve_names(x)
  expect_equal(length(nm), 3)
  expect_equal(length(unique(nm)), 3)
})

test_that("sby_internal_resolve_column_names cria nomes deterministas para matrix sem colnames", {
  m <- matrix(1:6, ncol = 3)
  expect_identical(resolve_names(m), c("v001", "v002", "v003"))
})

test_that("sby_internal_eval_select aplica defaults e selecao explicita", {
  df <- data.frame(a = 1:3, b = c(10, 20, 30), c = 4:6, d = c(TRUE, FALSE, TRUE))
  expect_identical(unname(eval_select(df, default = "all")), 1:4)
  expect_identical(unname(eval_select(df, default = "numeric")), 1:4)
  expect_identical(names(eval_select(df, a, c, default = "all")), c("a", "c"))
})

test_that("sby_internal_eval_select em matrix rejeita tidyselect", {
  m <- matrix(1:6, ncol = 2)
  expect_error(eval_select(m, 1, default = "all"), "not supported")
  expect_identical(eval_select(m, default = "all"), 1:2)
})

test_that("sby_internal_is_numeric_column valida vetores numericos/logicos e rejeita estruturas com dimensao", {
  expect_true(is_numeric_column(1:3))
  expect_true(is_numeric_column(c(1, NA_real_)))
  expect_true(is_numeric_column(TRUE))
  expect_false(is_numeric_column(matrix(1:4, ncol = 2)))
})


test_that("sby_internal_validate_threshold_scalar aceita limites e rejeita invalidos", {
  expect_identical(validate_threshold(0, "threshold"), 0)
  expect_identical(validate_threshold(1, "threshold"), 1)
  expect_error(validate_threshold(-1, "threshold"), "between 0 and 1")
  expect_error(validate_threshold(Inf, "threshold"), "finite")
  expect_error(validate_threshold(c(0.1, 0.2), "threshold"), "scalar")
})

test_that("sby_internal_validate_correlation_threshold delega validacao de faixa", {
  expect_identical(validate_corr_threshold(0.4), 0.4)
  expect_error(validate_corr_threshold(NA_real_), "non-missing")
  expect_error(validate_corr_threshold(2), "between 0 and 1")
})
