#' @title Gerar DDL Oracle a partir de um catálogo de dados
#'
#' @usage sby_dba_create_table(profile_data_catalog, table_name)
#'
#' @description
#' Gera uma instrução SQL `CREATE TABLE` para Oracle 19c usando os nomes e os
#' tipos Oracle produzidos por [sby_profile_data_catalog()].
#'
#' @details
#' `profile_data_catalog` deve ser um `data.frame`, `tibble` ou `data.table`,
#' preservar uma linha para cada coluna perfilada e conter os campos
#' `NOME_COLUNA` e `SUGESTAO_TIPO_ORACLE`. Os nomes das colunas são delimitados
#' com aspas duplas para preservar exatamente os identificadores da tabela de
#' origem, inclusive caixa, espaços e aspas internas. O nome da tabela é
#' validado como identificador Oracle não delimitado e convertido para
#' maiúsculas.
#'
#' A ordem das linhas do catálogo determina a ordem das colunas no SQL. Tipos
#' ausentes, vazios, com quebras de linha ou marcados como `REVISAR TIPO` não
#' geram SQL: nesses casos a função interrompe a execução para não produzir DDL
#' incompleto ou inseguro. A função não executa o comando no banco de dados.
#'
#' @param profile_data_catalog `data.frame`, `tibble` ou `data.table` retornado
#' por [sby_profile_data_catalog()].
#' @param table_name String única com o nome não delimitado da tabela Oracle a
#' criar. Deve começar com uma letra, conter no máximo 128 bytes e usar
#' somente letras, números, `_`, `$` ou `#`.
#'
#' @return String escalar, identada em múltiplas linhas, com a instrução SQL
#' `CREATE TABLE` e todas as colunas do catálogo, terminada por ponto e vírgula.
#'
#' @examples
#' catalogo <- sby_profile_data_catalog(
#'   data.frame(ID = 1:3, DESCRICAO = c("A", "B", "C"))
#' )
#' sby_dba_create_table(catalogo, "MINHA_TABELA")
#'
#' @seealso [sby_profile_data_catalog()]
#' @export
sby_dba_create_table <- function(profile_data_catalog, table_name){
  if(!inherits(profile_data_catalog, "data.frame")){
    stop(
      paste0(
        "`profile_data_catalog` must be a data.frame, tibble, or data.table ",
        "returned by sby_profile_data_catalog()"
      ),
      call. = FALSE
    )
  }

  requiredColumns <- c("NOME_COLUNA", "SUGESTAO_TIPO_ORACLE")
  missingColumns <- setdiff(requiredColumns, names(profile_data_catalog))

  if(length(missingColumns) > 0L){
    stop(
      str_c(
        "`profile_data_catalog` is missing required columns: ",
        str_c(missingColumns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if(nrow(profile_data_catalog) == 0L){
    stop(
      "`profile_data_catalog` must contain at least one profiled column",
      call. = FALSE
    )
  }

  if(
    !is.character(table_name) ||
      length(table_name) != 1L ||
      is.na(table_name) ||
      stri_numbytes(table_name) > 128L ||
      !str_detect(table_name, "^[[:alpha:]][[:alnum:]_$#]{0,127}$")
  ){
    stop("`table_name` must be a valid unquoted Oracle identifier", call. = FALSE)
  }

  columnNames <- profile_data_catalog$NOME_COLUNA
  oracleTypes <- profile_data_catalog$SUGESTAO_TIPO_ORACLE
  validOracleType <- if(is.character(oracleTypes)){
    str_detect(
      oracleTypes,
      paste0(
        "^(?:DATE|TIMESTAMP\\(6\\)|BINARY_DOUBLE|CLOB|",
        "NUMBER\\((?:[1-9]|[12][0-9]|3[0-8]),0\\)|",
        "VARCHAR2\\((?:[1-9][0-9]{0,2}|[1-3][0-9]{3}|4000) CHAR\\)|",
        "RAW\\(1\\))$"
      )
    )
  } else {
    FALSE
  }

  if(
    !is.character(columnNames) ||
      anyNA(columnNames) ||
      any(columnNames == "") ||
      any(stri_numbytes(columnNames) > 128)
  ){
    stop("`NOME_COLUNA` must contain non-empty Oracle 19c identifiers of at most 128 bytes", call. = FALSE)
  }

  if(anyDuplicated(columnNames)){
    stop("`NOME_COLUNA` must not contain duplicate column names", call. = FALSE)
  }

  if(
    !is.character(oracleTypes) ||
      anyNA(oracleTypes) ||
      any(!nzchar(oracleTypes)) ||
      any(oracleTypes == "REVISAR TIPO") ||
      !all(validOracleType)
  ){
    stop("`SUGESTAO_TIPO_ORACLE` contains a type that requires review", call. = FALSE)
  }

  quotedNames <- str_c(
    "\"",
    gsub("\"", "\"\"", columnNames, fixed = TRUE),
    "\""
  )
  columnDefinitions <- str_c("  ", quotedNames, " ", oracleTypes)

  str_c(
    "CREATE TABLE ",
    str_to_upper(table_name),
    " (\n",
    str_c(columnDefinitions, collapse = ",\n"),
    "\n);"
  )
}
