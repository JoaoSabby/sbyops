#' @title Estimate Average Row Width for Parquet Writing
#'
#' @description
#' Estimates the approximate average row width to define the row group size used
#' during Parquet writing.
#'
#' @details
#' The estimate is approximate and is used to choose \code{chunk_size}. For
#' variable-width columns, such as character columns, the function uses a
#' deterministic row sample to avoid excessive overhead during writing.
#'
#' @param .data Object of class \code{data.frame} or \code{tibble}.
#'
#' @return Numeric value with the approximate average row width in bytes.
#'
#' @keywords internal
sby_table_internal_row_width <- function(.data){
  
  # Obtain the number of rows with a fast collapse function
  row_count <- fnrow(.data)
  
  # Use a minimal estimate for empty tables
  if(row_count == 0L){
    return(1)
  }
  
  # Use a deterministic sample to estimate character width
  sample_size <- min(row_count, 10000L)
  
  sample_index <- unique(as.integer(round(seq.int(1L, row_count, length.out = sample_size))))
  
  # Sum per-column width estimates
  row_width <- sum(vapply(.data, function(current_column){
    column_class <- class(current_column)[1L]
    base_type <- typeof(current_column)
    
    # Date columns use a compact Arrow representation
    if(column_class == "Date"){
      return(4)
    }
    
    # Factors are treated as integer codes plus a dictionary
    if(column_class == "factor"){
      return(4)
    }
    
    # Logical vectors use bit packing and a validity bitmap
    if(base_type == "logical"){
      return(0.25)
    }
    
    # R integer vectors use up to 32 bits before further optimization
    if(base_type == "integer"){
      return(4)
    }
    
    # Double vectors use 64 bits before further optimization
    if(base_type == "double"){
      return(8)
    }
    
    # Character vectors use value bytes plus offsets and validity information
    if(base_type == "character"){
      sample_values <- current_column[sample_index]
      sample_bytes <- nchar(sample_values, type = "bytes", allowNA = TRUE)
      mean_bytes <- mean(sample_bytes, na.rm = TRUE)
      
      if(is.na(mean_bytes)){
        mean_bytes <- 0
      }
      
      return(mean_bytes + 8)
    }
    
    # Use a conservative estimate for unexpected types
    return(8)
  }, FUN.VALUE = numeric(1L)))
  
  # Ensure a positive minimum width
  return(max(row_width, 1L))
}
####
## End
#
