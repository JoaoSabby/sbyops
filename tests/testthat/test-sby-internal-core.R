# Este arquivo cobre helpers internos de validacao, selecao e suporte de tipos.

validateTabular <- getFromNamespace("sby_internal_validate_tabular_input", "sbyops")
resolveNames <- getFromNamespace("sby_internal_resolve_column_names", "sbyops")
evalSelect <- getFromNamespace("sby_internal_eval_select", "sbyops")
isNumericColumn <- getFromNamespace("sby_internal_is_numeric_column", "sbyops")
isModalSupported <- getFromNamespace("sby_internal_is_modal_supported", "sbyops")
encodeModal <- getFromNamespace("sby_internal_encode_modal_column", "sbyops")
validateThreshold <- getFromNamespace("sby_internal_validate_threshold_scalar", "sbyops")
validateCorrThreshold <- getFromNamespace("sby_internal_validate_correlation_threshold", "sbyops")


test_that("sby_internal_validate_tabular_input aceita classes tabulares suportadas", {
  expect_identical(validateTabular(data.frame(a = 1)), data.frame(a = 1))
  expect_identical(validateTabular(matrix(1:4, ncol = 2)), matrix(1:4, ncol = 2))
})

test_that("sby_internal_validate_tabular_input rejeita objetos nao tabulares", {
  expect_error(validateTabular(1:3), "must be a data.frame")
  expect_error(validateTabular(list(a = 1)), "must be a data.frame")
  expect_error(validateTabular(NULL), "must be a data.frame")
})

test_that("sby_internal_resolve_column_names repara nomes vazios e duplicados", {
  x <- data.frame(" " = 1, a = 2, a = 3, check.names = FALSE)
  nm <- resolveNames(x)
  expect_equal(length(nm), 3)
  expect_equal(length(unique(nm)), 3)
})

test_that("sby_internal_resolve_column_names cria nomes deterministas para matrix sem colnames", {
  m <- matrix(1:6, ncol = 3)
  expect_identical(resolveNames(m), c("v001", "v002", "v003"))
})

test_that("sby_internal_eval_select aplica defaults e selecao explicita", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"), c = 4:6)
  expect_identical(unname(evalSelect(df, default = "all")), 1:3)
  expect_identical(unname(evalSelect(df, default = "numeric")), c(1L, 3L))
  expect_identical(names(evalSelect(df, a, c, default = "all")), c("a", "c"))
})

test_that("sby_internal_eval_select em matrix rejeita tidyselect", {
  m <- matrix(1:6, ncol = 2)
  expect_error(evalSelect(m, 1, default = "all"), "not supported")
  expect_identical(evalSelect(m, default = "all"), 1:2)
})

test_that("sby_internal_is_numeric_column valida vetores numericos e rejeita estruturas com dimensao", {
  expect_true(isNumericColumn(1:3))
  expect_true(isNumericColumn(c(1, NA_real_)))
  expect_false(isNumericColumn(TRUE))
  expect_false(isNumericColumn(matrix(1:4, ncol = 2)))
})

test_that("sby_internal_is_modal_supported cobre tipos suportados e nao suportados", {
  expect_true(isModalSupported(c("a", NA_character_)))
  expect_true(isModalSupported(c(1L, 2L)))
  expect_true(isModalSupported(c(TRUE, FALSE)))
  expect_false(isModalSupported(matrix(1:4, ncol = 2)))
  expect_false(isModalSupported(I(list(1, 2))))
})

test_that("sby_internal_encode_modal_column retorna codigos inteiros estaveis", {
  x <- factor(c("a", NA, "a", "b"), levels = c("a", "b", "z"))
  res <- encodeModal(x)
  expect_type(res, "list")
  expect_true(is.integer(res$codes))
  expect_equal(length(res$codes), length(x))
  expect_true(res$max_code >= 3L)
})

test_that("sby_internal_validate_threshold_scalar aceita limites e rejeita invalidos", {
  expect_identical(validateThreshold(0, "threshold"), 0)
  expect_identical(validateThreshold(1, "threshold"), 1)
  expect_error(validateThreshold(-1, "threshold"), "between 0 and 1")
  expect_error(validateThreshold(Inf, "threshold"), "finite")
  expect_error(validateThreshold(c(0.1, 0.2), "threshold"), "scalar")
})

test_that("sby_internal_validate_correlation_threshold delega validacao de faixa", {
  expect_identical(validateCorrThreshold(0.4), 0.4)
  expect_error(validateCorrThreshold(NA_real_), "non-missing")
  expect_error(validateCorrThreshold(2), "between 0 and 1")
})
