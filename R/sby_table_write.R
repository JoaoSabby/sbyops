#' @title Escrever tabela em Parquet com schema Arrow otimizado
#'
#' @description
#' Recebe um data frame, tibble ou data.table, aplica otimização automática de schema Arrow e
#' grava o resultado em arquivo Parquet.
#'
#' @details
#' A ferramenta centraliza parâmetros de escrita analítica: schema otimizado,
#' compressão, dictionary encoding por coluna, estatísticas, tamanho de página,
#' row group e versão Parquet. O codec de compressão pode ser sobrescrito por
#' `options(sby_parquet_compression = ...)`.
#'
#' @param .data Tabela DuckDB, data frame, tibble ou data.table a ser escrito.
#' @param file Caminho completo de saída. A extensão `.parquet` é adicionada
#' automaticamente quando ausente.
#'
#' @return Retorna `NULL` invisivelmente.
#'
#' @usage sby_table_write(.data, file)
#'
#' @examples
#' \dontrun{
#' sby_table_write(.data = dados, file = "dados.parquet")
#' }
#'
#' @seealso [sby_table_optimize_scheme()]
#' @export
sby_table_write <- function(.data, file){

  # Collect lazy DuckDB relations once for schema inference and Parquet writing.
  .data <- sby_internal_validate_tabular_input(.data = .data)

  # Require arrow only when Parquet writing is requested
  if(!requireNamespace("arrow", quietly = TRUE)){
    stop("O pacote 'arrow' é necessário para usar sby_table_write(). Instale com install.packages('arrow').")
  }

  # Normalize the output file extension
  file <- sby_internal_table_normalize_file(file)

  # Create the destination directory when needed
  file_dir <- dirname(file)

  if(!dir.exists(file_dir)){
    dir.create(file_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Check whether the destination directory is available
  if(!dir.exists(file_dir)){
    stop(str_c("It was not possible to create the destination directory: ", file_dir))
  }

  # Create an optimized schema before Arrow table materialization
  schema_arrow <- sby_table_optimize_scheme(.data)

  # Define internal writing parameters
  compression <- sby_internal_table_compression()
  use_dictionary <- sby_internal_table_dictionary(.data)
  chunk_size <- sby_internal_table_chunk_size(.data)
  data_page_size <- getOption("sby_parquet_data_page_size", 2097152L)
  parquet_version <- getOption("sby_parquet_version", "2.6")
  write_statistics <- TRUE


  # Create an Arrow table with an explicit schema
  table_arrow <- arrow::arrow_table(
    .data,
    schema = schema_arrow
  )

  # Write parquet with automatic parameters
  arrow::write_parquet(
    x = table_arrow,
    sink = file,
    chunk_size = chunk_size,
    version = parquet_version,
    compression = compression,
    compression_level = NULL,
    use_dictionary = use_dictionary,
    write_statistics = write_statistics,
    data_page_size = data_page_size,
    use_deprecated_int96_timestamps = FALSE,
    coerce_timestamps = NULL,
    allow_truncated_timestamps = FALSE
  )

  # Return quietly for pipeline-friendly usage
  invisible()
}
####
## End
#
