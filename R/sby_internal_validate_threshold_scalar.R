#' @title Internal Helper: Validate Threshold Scalar
#'
#' @usage sby_internal_validate_threshold_scalar(threshold, arg_name)
#'
#' @description Validate threshold scalar used by selector functions
#'
#' @param threshold Numeric threshold candidate
#'
#' @param arg_name Argument name used in error messages
#'
#' @return A validated numeric scalar threshold
sby_internal_validate_threshold_scalar <- function(threshold, arg_name){

  # Abort when threshold argument is missing
  if(missing(threshold)){
    stop(sprintf("`%s` is required", arg_name), call. = FALSE)
  }

  # Abort when threshold is not a non-missing numeric scalar
  if(!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold)){
    stop(sprintf("`%s` must be a non-missing numeric scalar", arg_name), call. = FALSE)
  }

  # Abort when threshold value is not finite
  if(!is.finite(threshold)){
    stop(sprintf("`%s` must be finite", arg_name), call. = FALSE)
  }

  # Abort when threshold value is outside closed interval [0, 1]
  if(threshold < 0 || threshold > 1){
    stop(sprintf("`%s` must be between 0 and 1", arg_name), call. = FALSE)
  }

  # Build validated threshold value for explicit return
  validated_threshold <- as.numeric(threshold)

  # Return validated numeric threshold scalar
  return(validated_threshold)
}
####
## End
# 
