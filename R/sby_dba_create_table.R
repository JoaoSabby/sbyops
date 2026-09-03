#' @title Gerar DDL Oracle a partir de um catálogo de dados
#'
#' @usage sby_dba_create_table(.data, table_name)
#'
#' @description
#' Gera uma instrução SQL `CREATE TABLE` para Oracle 19c usando os nomes e os
#' tipos Oracle produzidos por [sby_profile_data_catalog()].
#'
#' @details
#' `.data` deve preservar uma linha para cada coluna perfilada e conter os
#' campos `NOME_COLUNA` e `SUGESTAO_TIPO_ORACLE`. Os nomes das colunas são delimitados
#' com aspas duplas para preservar exatamente os identificadores da tabela de
#' origem. O nome da tabela é validado como identificador Oracle não delimitado
#' e convertido para maiúsculas.
#'
#' Tipos que requerem revisão manual não geram SQL. A função interrompe a
#' execução quando o catálogo contém um tipo ausente ou `REVISAR TIPO`.
#'
#' @param .data Tibble retornado por [sby_profile_data_catalog()].
#' @param table_name Nome não delimitado da tabela Oracle a criar.
#'
#' @return String com a instrução SQL `CREATE TABLE`, terminada por ponto e
#' vírgula.
#'
#' @examples
#' catalogo <- sby_profile_data_catalog(
#'   data.frame(ID = 1:3, DESCRICAO = c("A", "B", "C"))
#' )
#' sby_dba_create_table(catalogo, "MINHA_TABELA")
#'
#' @seealso [sby_profile_data_catalog()]
#' @export
sby_dba_create_table <- function(.data, table_name){
  if(!inherits(.data, "tbl_df")){
    stop("`.data` must be a tibble returned by sby_profile_data_catalog()", call. = FALSE)
  }

  requiredColumns <- c("NOME_COLUNA", "SUGESTAO_TIPO_ORACLE")
  missingColumns <- setdiff(requiredColumns, names(.data))

  if(length(missingColumns) > 0L){
    stop(
      str_c(
        "`.data` is missing required columns: ",
        str_c(missingColumns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if(nrow(.data) == 0L){
    stop("`.data` must contain at least one profiled column", call. = FALSE)
  }

  if(
    !is.character(table_name) ||
      length(table_name) != 1L ||
      is.na(table_name) ||
      !str_detect(table_name, "^[[:alpha:]][[:alnum:]_$#]{0,127}$")
  ){
    stop("`table_name` must be a valid unquoted Oracle identifier", call. = FALSE)
  }

  columnNames <- .data$NOME_COLUNA
  oracleTypes <- .data$SUGESTAO_TIPO_ORACLE

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
      any(oracleTypes == "REVISAR TIPO")
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
