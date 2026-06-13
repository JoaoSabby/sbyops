# Cenarios basicos somente com integer e double
test_that("sby_select_non_constant removes constant columns", {
  df <- data.frame(
    x = 1:5,
    y = rep(2L, 5),
    z = rep(3.5, 5),
    w = c(1L, 0L, 1L, 0L, 1L)
  )

  out <- sby_select_non_constant(df)
  expect_named(out, c("x", "w"))
})

# Garante que a selecao por tidyselect continua respeitada
test_that("sby_select_non_constant respects tidyselect", {
  df <- data.frame(
    a = rep(1, 4),
    b = 1:4,
    c = rep(9.0, 4)
  )

  out <- sby_select_non_constant(df, a:c)
  expect_named(out, "b")

  out2 <- sby_select_non_constant(df, b)
  expect_named(out2, c("a", "b", "c"))
})

# Mantem retorno como matrix quando entrada e matrix
test_that("sby_select_non_constant supports matrix input", {
  mat <- cbind(
    a = 1:4,
    b = rep(3, 4),
    c = c(1, 1, 2, 2)
  )

  out <- sby_select_non_constant(mat)
  expect_true(is.matrix(out))
  expect_equal(colnames(out), c("a", "c"))
})

# Confere comportamento para NA e NaN em colunas numericas
test_that("sby_select_non_constant handles NA and NaN as constant when all are missing", {
  df <- data.frame(
    all_na = c(NA_real_, NA_real_, NA_real_),
    all_nan = c(NaN, NaN, NaN),
    mix_na_value = c(NA_real_, 1, NA_real_),
    mix_nan_value = c(NaN, 2, NaN),
    varying = c(1, 2, 3)
  )

  out <- sby_select_non_constant(df)
  expect_false("all_na" %in% names(out))
  expect_false("all_nan" %in% names(out))
  expect_true("mix_na_value" %in% names(out))
  expect_true("mix_nan_value" %in% names(out))
  expect_true("varying" %in% names(out))
})
