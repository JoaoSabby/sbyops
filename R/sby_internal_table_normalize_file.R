#' @title Normalize a Parquet File Path
#'
#' @description
#' Normalizes the output path to ensure that the file extension is
#' \code{.parquet}.
#'
#' @param file Complete output file path.
#'
#' @return File path with the \code{.parquet} extension.
#'
#' @keywords internal
sby_internal_table_normalize_file <- function(file){
  
  # Validate the file path provided by the user
  if(missing(file) || !is.character(file) || length(file) != 1L || is.na(file)){
    stop("The file argument must be a valid file path")
  }
  
  # Preserve the extension when it is already provided
  if(grepl("\\.parquet$", file, ignore.case = TRUE)){
    return(file)
  }
  
  # Add the parquet extension when it is absent
  return(str_c(file, ".parquet"))
}
####
## End
#