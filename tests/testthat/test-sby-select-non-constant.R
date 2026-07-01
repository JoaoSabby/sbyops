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

test_that("sby_select_non_constant validates column types only after tidyselect", {
  df <- data.frame(
    TARGET = c("x", "y", "z", "w"),
    colunasId = c("id1", "id2", "id3", "id4"),
    constant_numeric = rep(1L, 4),
    varying_numeric = 1:4
  )

  out <- sby_select_non_constant(df, -TARGET, -colunasId)
  expect_named(out, c("TARGET", "colunasId", "varying_numeric"))

  expect_error(
    sby_select_non_constant(df, TARGET),
    "`.data` must contain only integer, double, or logical columns",
    fixed = TRUE
  )
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


test_that("sby_select_non_constant supports injected character variables with negative tidyselect", {
  df <- data.frame(
    TARGET = c(0L, 1L, 0L, 1L),
    xyz = c(10L, 11L, 12L, 13L),
    feature = c(2L, 3L, 4L, 5L),
    DATA_REFERENCIA_TARGET = c("2026-01-01", "2026-01-02", "2026-01-03", "2026-01-04")
  )
  variaveis <- "xyz"
  columnsKeyContext <- c("TARGET", "DATA_REFERENCIA_TARGET")

  out <- sby_select_non_constant(
    df,
    -TARGET,
    -!!variaveis,
    -!!columnsKeyContext
  )

  expect_named(out, c("TARGET", "xyz", "feature", "DATA_REFERENCIA_TARGET"))
})

test_that("sby_select_non_constant accepts logical columns", {
  df <- data.frame(
    constant_true = c(TRUE, TRUE, TRUE, TRUE),
    constant_na = c(NA, NA, NA, NA),
    varying_logical = c(TRUE, FALSE, TRUE, FALSE),
    mixed_missing = c(TRUE, NA, TRUE, TRUE),
    numeric_keep = 1:4
  )

  out <- sby_select_non_constant(df)

  expect_named(out, c("varying_logical", "mixed_missing", "numeric_keep"))
})

test_that("sby_select_non_constant ignores absent injected character selectors", {
  df <- data.frame(
    TARGET = c(0L, 1L, 0L, 1L),
    feature = 1:4,
    constant = rep(7L, 4)
  )
  columnsKeyContext <- c(
    "SEQUENCIA_CLIENTE",
    "DATA_REFERENCIA_TARGET",
    "TARGET"
  )

  out <- sby_select_non_constant(
    df,
    TARGET,
    -!!columnsKeyContext
  )
  expect_named(out, c("TARGET", "feature", "constant"))

  out2 <- sby_select_non_constant(
    df,
    -!!columnsKeyContext
  )
  expect_named(out2, c("TARGET", "feature"))
})
