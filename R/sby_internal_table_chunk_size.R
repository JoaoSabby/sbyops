#' @title Compute Parquet Row Group Size
#'
#' @description
#' Automatically computes \code{chunk_size} for Parquet writing with an emphasis
#' on fast analytical reads.
#'
#' @details
#' The function seeks row groups that are large enough to reduce metadata
#' overhead and favor sequential reads, while avoiding excessively large blocks
#' for wide tables.
#'
#' The default target is 512 MiB per row group. It can be changed with
#' \code{options(sby_parquet_row_group_bytes = ...)}.
#'
#' @param .data Object of class \code{data.frame} or \code{tibble}.
#'
#' @return Integer with the number of rows per row group.
#'
#' @keywords internal
sby_internal_table_chunk_size <- function(.data){
  
  # Obtain the number of rows with a fast collapse function
  row_count <- fnrow(.data)
  
  # Keep the Arrow internal choice for empty tables
  if(row_count == 0L){
    return(NULL)
  }
  
  # Estimate average row width
  row_width <- sby_internal_table_row_width(.data)
  
  # Define the target number of bytes per row group through an internal option
  target_row_group_bytes <- getOption("sby_parquet_row_group_bytes", 536870912)
  
  # Compute rows per row group from the estimated row width
  chunk_size <- floor(target_row_group_bytes / row_width)
  
  # Avoid excessively small row groups in wide tables
  chunk_size <- max(chunk_size, 100000L)
  
  # Avoid excessively large row groups in narrow tables
  chunk_size <- min(chunk_size, 1000000L)
  
  # Do not exceed the available number of rows
  chunk_size <- min(chunk_size, row_count)
  
  return(as.integer(chunk_size))
}
####
## Fim
#