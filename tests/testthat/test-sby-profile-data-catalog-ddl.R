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
})
