#' @title Write a Table to Parquet with an Optimized Arrow Schema
#'
#' @description
#' Receives a \code{data.frame} or \code{tibble}, applies automatic Arrow schema
#' optimization, and writes the result to a Parquet file.
#'
#' @details
#' The function receives only \code{.data} and \code{file}. All writing
#' parameters are defined internally with an emphasis on fast analytical reading
#' and practical use.
#'
#' The function applies an optimized Arrow schema, defines fast compression,
#' column-level dictionary encoding, statistics, data page size, row group size,
#' and Parquet version.
#'
#' The Parquet version is obtained from
#' \code{getOption("sby_parquet_version", "2.6")}. The compression codec can be
#' overridden with \code{options(sby_parquet_compression = ...)}.
#'
#' @param .data Object of class \code{data.frame} or \code{tibble} to be
#' written.
#' @param file Complete output file path. The \code{.parquet} extension is added
#' automatically when absent.
#'
#' @return
#' Invisibly returns \code{NULL}.
#'
#' @usage sby_table_write(.data, file)
#'
#' @examples
#' \dontrun{
#' sby_table_write(
#'   .data = data,
#'   file = "/data/modeling/training_base"
#' )
#'
#' sby_table_write(
#'   .data = data,
#'   file = "/data/modeling/training_base.parquet"
#' )
#' }
#'
#'
#' @export
sby_table_write <- function(.data, file){

  # Require arrow only when Parquet writing is requested
  if(!requireNamespace("arrow", quietly = TRUE)){
    stop("O pacote 'arrow' é necessário para usar sby_table_write(). Instale com install.packages('arrow').")
  }

  # Validate the main structure before creating Arrow objects
  if(!is.data.frame(.data)){
    stop("The object must be a data.frame or tibble")
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
