#' @title Optimize Arrow Schema for Parquet Writing
#'
#' @description
#' Inspects the columns of a \code{data.frame} or \code{tibble} and returns an
#' optimized Arrow \code{Schema} for Parquet writing.
#'
#' @details
#' The function defines compact Arrow types from the observed content of each
#' column. Logical variables are mapped to \code{boolean()}, character variables
#' to \code{utf8()}, factors to \code{dictionary()}, and dates to
#' \code{date32()}.
#'
#' For \code{double} columns, the function uses
#' \code{sby_table_internal_detect_numeric_type()}, implemented in C++ through
#' Rcpp, to collect metadata in a single pass.
#'
#' For integer columns, the function uses
#' \code{sby_table_internal_detect_integer_type()}, also implemented in C++,
#' to avoid separate calls for minimum, maximum, and boolean feasibility checks.
#'
#' Integer type compaction respects the representation limits of Arrow integer
#' types. Values \code{Inf} and \code{-Inf} prevent conversion to integer
#' types. Integer-like \code{double} values are compacted only within the
#' exact-representation range (\code{2^.Machine$double.digits}); values above
#' this bound are preserved as \code{float64()}.
#'
#' @param .data Object of class \code{data.frame} or \code{tibble} containing
#' the columns to be analyzed.
#'
#' @return Arrow \code{Schema} object.
#'
#' @usage sby_table_optimize_scheme(.data)
#'
#' @examples
#' \dontrun{
#' schema_arrow <- sby_table_optimize_scheme(.data = data)
#' }
#'
#' @importFrom arrow schema boolean utf8 dictionary date32 int8 int16 int32 int64 uint8 uint16 uint32 uint64 float64
#' @importFrom stringr str_c
#'
#' @export
sby_table_optimize_scheme <- function(.data){
  
  # Validate the main structure before inferring types
  if(!is.data.frame(.data)){
    stop("The object must be a data.frame or tibble")
  }
  
  # Check whether all columns belong to supported types
  valid_column <- vapply(.data, function(current_column){
    column_class <- class(current_column)[1L]
    base_type <- typeof(current_column)
    
    base_type %in% c("integer", "double", "character", "logical") ||
      column_class %in% c("factor", "Date")
  }, FUN.VALUE = logical(1L))
  
  # Stop early to avoid a partial or incorrect schema
  if(!all(valid_column)){
    invalid_columns <- names(.data)[!valid_column]
    
    stop(
      str_c(
        "The object contains columns with unsupported types: ",
        str_c(invalid_columns, collapse = ", ")
      )
    )
  }
  
  # Define the limit for integers exactly represented as double values
  exact_integer_limit <- 2 ^ .Machine$double.digits
  
  # Infer the most compact Arrow type for each column
  arrow_types <- lapply(.data, function(current_column){
    column_class <- class(current_column)[1L]
    base_type <- typeof(current_column)
    
    # Store factors as Arrow dictionaries
    if(column_class == "factor"){
      return(dictionary())
    }
    
    # R Date vectors are compatible with date32
    if(column_class == "Date"){
      return(date32())
    }
    
    # Use utf8 for broad character compatibility
    if(base_type == "character"){
      return(utf8())
    }
    
    # Native logical vectors map directly to boolean
    if(base_type == "logical"){
      return(boolean())
    }
    
    # Native integer vectors are evaluated by a C routine
    if(base_type == "integer"){
      integer_metadata <- sby_table_internal_detect_integer_type(current_column)
      
      has_value <- integer_metadata[[1L]] == 1
      is_boolean <- integer_metadata[[2L]] == 1
      min_value <- integer_metadata[[3L]]
      max_value <- integer_metadata[[4L]]
      
      # Columns without valid values keep the default R integer type
      if(!has_value){
        return(int32())
      }
      
      # Integers restricted to 0 and 1 can be represented as booleans
      if(is_boolean){
        return(boolean())
      }
      
      # Negative values require signed integers
      if(min_value < 0){
        if(min_value >= -128 && max_value <= 127){
          return(int8())
        }
        
        if(min_value >= -32768 && max_value <= 32767){
          return(int16())
        }
        
        return(int32())
      }
      
      # Non-negative values allow unsigned integers
      if(max_value <= 255){
        return(uint8())
      }
      
      if(max_value <= 65535){
        return(uint16())
      }
      
      return(uint32())
    }
    
    # Double vectors require complete metadata before compaction
    if(base_type == "double"){
      numeric_metadata <- sby_table_internal_detect_numeric_type(current_column)
      
      has_value <- numeric_metadata[[1L]] == 1
      has_non_finite <- numeric_metadata[[2L]] == 1
      is_integer <- numeric_metadata[[3L]] == 1
      is_boolean <- numeric_metadata[[4L]] == 1
      min_value <- numeric_metadata[[5L]]
      max_value <- numeric_metadata[[6L]]
      
      # Empty columns or columns with infinities remain float64
      if(!has_value || has_non_finite){
        return(float64())
      }
      
      # Doubles restricted to 0 and 1 can be represented as booleans
      if(is_boolean){
        return(boolean())
      }
      
      # Integer-like doubles are compacted only within the exact range
      if(is_integer){
        if(abs(min_value) > exact_integer_limit || abs(max_value) > exact_integer_limit){
          return(float64())
        }
        
        if(min_value < 0){
          if(min_value >= -128 && max_value <= 127){
            return(int8())
          }
          
          if(min_value >= -32768 && max_value <= 32767){
            return(int16())
          }
          
          if(min_value >= -2147483648 && max_value <= 2147483647){
            return(int32())
          }
          
          return(int64())
        }
        
        if(max_value <= 255){
          return(uint8())
        }
        
        if(max_value <= 65535){
          return(uint16())
        }
        
        if(max_value <= 4294967295){
          return(uint32())
        }
        
        return(uint64())
      }
      
      # Continuous numeric values remain Arrow doubles
      return(float64())
    }
  })
  
  # Preserve original column names in the final schema
  names(arrow_types) <- names(.data)
  
  # Build the Arrow schema for writing
  schema_arrow <- do.call(schema, arrow_types)
  
  return(schema_arrow)
}
####
## End
#
