test_that("sby_profile_data_catalog profiles every input column", {
  inputData <- data.frame(
    id = 1:3,
    description = c("A", "B", "C"),
    active = c(TRUE, FALSE, TRUE)
  )

  catalog <- sby_profile_data_catalog(inputData)

  expect_s3_class(catalog, "tbl_df")
  expect_identical(nrow(catalog), ncol(inputData))
  expect_identical(names(catalog)[[1L]], "NOME_COLUNA")
  expect_identical(catalog$NOME_COLUNA, names(inputData))
  expect_true(all(names(catalog) == toupper(names(catalog))))
  expect_true(all(grepl("^[A-Z][A-Z0-9_]*$", names(catalog))))
})

test_that("sby_dba_create_table builds Oracle DDL from a catalog", {
  catalog <- tibble(
    NOME_COLUNA = c("ID", "DESCRICAO"),
    SUGESTAO_TIPO_ORACLE = c("NUMBER(1,0)", "VARCHAR2(20 CHAR)")
  )

  ddl <- sby_dba_create_table(catalog, "minha_tabela")

  expect_identical(
    ddl,
    paste0(
      "CREATE TABLE MINHA_TABELA (\n",
      "  \"ID\" NUMBER(1,0),\n",
      "  \"DESCRICAO\" VARCHAR2(20 CHAR)\n",
      ");"
    )
  )
})

test_that("sby_dba_create_table accepts every documented in-memory catalog class", {
  catalogDataFrame <- data.frame(
    NOME_COLUNA = c("id", "Nome completo", "valor\"auditado"),
    SUGESTAO_TIPO_ORACLE = c(
      "NUMBER(1,0)",
      "VARCHAR2(20 CHAR)",
      "BINARY_DOUBLE"
    )
  )

  expectedDdl <- paste0(
    "CREATE TABLE TABELA_AUDITORIA (\n",
    "  \"id\" NUMBER(1,0),\n",
    "  \"Nome completo\" VARCHAR2(20 CHAR),\n",
    "  \"valor\"\"auditado\" BINARY_DOUBLE\n",
    ");"
  )

  expect_identical(
    sby_dba_create_table(catalogDataFrame, "tabela_auditoria"),
    expectedDdl
  )
  expect_identical(
    sby_dba_create_table(as.data.table(catalogDataFrame), "tabela_auditoria"),
    expectedDdl
  )
})

test_that("sby_dba_create_table rejects incomplete or unresolved catalogs", {
  expect_error(
    sby_dba_create_table(tibble(NOME_COLUNA = "ID"), "TABELA"),
    "missing required columns"
  )
  expect_error(
    sby_dba_create_table(
      tibble(
        NOME_COLUNA = "OBJETO",
        SUGESTAO_TIPO_ORACLE = "REVISAR TIPO"
      ),
      "TABELA"
    ),
    "requires review"
  )
  expect_error(
    sby_dba_create_table(
      data.frame(
        NOME_COLUNA = "ID",
        SUGESTAO_TIPO_ORACLE = "NUMBER(1,0)); DROP TABLE CLIENTES; --"
      ),
      "TABELA"
    ),
    "requires review"
  )
  expect_error(
    sby_dba_create_table(
      data.frame(
        NOME_COLUNA = "ID",
        SUGESTAO_TIPO_ORACLE = "NUMBER(1,0)\nDROP TABLE CLIENTES"
      ),
      "TABELA"
    ),
    "requires review"
  )
  expect_error(
    sby_dba_create_table(list(
      NOME_COLUNA = "ID",
      SUGESTAO_TIPO_ORACLE = "NUMBER(1,0)"
    ), "TABELA"),
    "data.frame, tibble, or data.table"
  )
})

test_that("sby_profile_data_catalog distinguishes missing and non-finite values", {
  catalog <- sby_profile_data_catalog(
    data.frame(valor = c(NA_real_, NaN, Inf, -Inf, 0, 2))
  )

  expect_identical(catalog$QTD_REGISTROS, 6)
  expect_identical(catalog$QTD_PREENCHIDOS, 4)
  expect_identical(catalog$QTD_NULOS, 2)
  expect_identical(catalog$QTD_NA, 1)
  expect_identical(catalog$QTD_NAN, 1)
  expect_identical(catalog$QTD_INFINITO_POSITIVO, 1)
  expect_identical(catalog$QTD_INFINITO_NEGATIVO, 1)
  expect_identical(catalog$QTD_VALIDOS_ESTATISTICA, 2)
})

test_that("sby_profile_data_catalog validates controls and empty selections", {
  expect_error(
    sby_profile_data_catalog(data.frame(id = 1L), max_robust_sample = 0L),
    "positive integer scalar"
  )

  emptyCatalog <- sby_profile_data_catalog(data.frame(id = 1L), -id)
  expect_s3_class(emptyCatalog, "tbl_df")
  expect_identical(dim(emptyCatalog), c(0L, 0L))
})
