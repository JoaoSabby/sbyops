#' @title Define Dictionary Encoding for Parquet
#'
#' @description
#' Automatically defines which columns should use dictionary encoding during
#' Parquet writing.
#'
#' @details
#' Dictionary encoding is enabled for factors and for character columns with
#' low observed cardinality. The function scans all candidate columns fully,
#' which avoids incorrect decisions when rare categories exist.
#'
#' For numeric, logical, and date columns, dictionary encoding is disabled by
#' default to preserve direct primitive representation.
#'
#' @param .data Object of class \code{data.frame} or \code{tibble}.
#'
#' @return Named logical vector with one decision per column.
#'
#' @importFrom collapse fncol fnrow
#' @importFrom kit countNA uniqLen
#'
#' @keywords internal
sby_table_internal_dictionary <- function(.data){
  
  # Obtain the number of rows with a fast collapse function
  row_count <- fnrow(.data)
  
  # Return an empty vector when there are no columns
  if(length(.data) == 0L){
    return(logical(0L))
  }
  
  # Handle zero-row tables without measuring cardinality
  if(row_count == 0L){
    use_dictionary <- rep(FALSE, length(.data))
    names(use_dictionary) <- names(.data)
    
    return(use_dictionary)
  }
  
  # Define thresholds through internal options
  max_distinct <- getOption("sby_parquet_dictionary_max_distinct", 65536L)
  max_distinct_ratio <- getOption("sby_parquet_dictionary_max_ratio", 0.50)
  
  # Decide dictionary usage column by column
  use_dictionary <- vapply(.data, function(current_column){
    column_class <- class(current_column)[1L]
    base_type <- typeof(current_column)
    
    # Factors already represent categories and are natural candidates
    if(column_class == "factor"){
      return(TRUE)
    }
    
    # Only character vectors are evaluated by cardinality
    if(base_type != "character"){
      return(FALSE)
    }
    
    # Count missing values without creating an intermediate logical vector
    na_count <- countNA(current_column)
    valid_count <- row_count - na_count
    
    # Fully missing character columns can use dictionary encoding with low cost
    if(valid_count == 0L){
      return(TRUE)
    }
    
    # Compute real cardinality by scanning the full column
    distinct_count <- uniqLen(current_column)
    
    # Adjust cardinality when NA was counted as a distinct value
    if(na_count > 0L){
      distinct_count <- distinct_count - 1L
    }
    
    # Compute the distinct ratio among valid values
    distinct_ratio <- distinct_count / valid_count
    
    # Enable dictionary only when repetition is sufficiently high
    return(distinct_count <= max_distinct && distinct_ratio <= max_distinct_ratio)
  }, FUN.VALUE = logical(1L))
  
  # Preserve names for direct use in write_parquet
  names(use_dictionary) <- names(.data)
  
  return(use_dictionary)
}
####
## Fim
#