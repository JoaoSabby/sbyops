#' @title Define Parquet Compression Codec
#'
#' @description
#' Automatically defines the compression codec with an emphasis on read and
#' write speed.
#'
#' @details
#' The \code{snappy} codec is preferred when available. Otherwise, the function
#' uses \code{uncompressed}, which is always supported.
#'
#' The decision can be overridden with
#' \code{options(sby_parquet_compression = ...)}.
#'
#' @return Selected codec name.
#'
#' @importFrom arrow codec_is_available
#'
#' @keywords internal
sby_table_internal_compression <- function(){
  
  # Obtain the codec defined by an advanced user
  compression <- getOption("sby_parquet_compression", NULL)
  
  # Use the provided codec when it is available
  if(!is.null(compression)){
    if(compression == "uncompressed"){
      return("uncompressed")
    }
    
    if(codec_is_available(compression)){
      return(compression)
    }
    
    stop(str_c("Unavailable compression codec: ", compression))
  }
  
  # Prefer snappy for its balance between CPU cost and IO reduction
  if(codec_is_available("snappy")){
    return("snappy")
  }
  
  # Use uncompressed output when snappy is unavailable
  return("uncompressed")
}
####
## Fim
#
