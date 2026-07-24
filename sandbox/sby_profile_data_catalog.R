#' @title Profile Tabular Data for Modeling and Oracle
#'
#' @usage
#' sby_profile_data_catalog(
#'   .data,
#'   ...,
#'   bitmap_cardinality_ratio = 0.01,
#'   bitmap_minimum_rows = 100000L,
#'   partition_minimum_rows = 1000000L,
#'   max_robust_sample = 100000L,
#'   num_treads = NULL
#' )
#'
#' @description
#' Build a column-level data catalog with descriptive statistics, missingness,
#' cardinality, text quality, outlier diagnostics, model-readiness flags, and
#' data-only Oracle recommendations.
#'
#' @details
#' The input is converted to a private `data.table` container and is never
#' modified by reference. The result is always a tibble. Oracle index,
#' histogram, compression, and partitioning fields are screening signals.
#' Workload metadata and database optimizer statistics remain required before
#' applying physical changes.
#'
#' Parallel resources are controlled by the existing sbyops thread context.
#' In a dynamically branched `targets` pipeline, set `num_treads = 1L` unless
#' worker-level resource allocation explicitly reserves more threads.
#'
#' @param .data A data frame or tibble.
#' @param ... Tidyselect expressions. When omitted, all columns are profiled.
#' @param bitmap_cardinality_ratio Maximum distinct-value ratio used only to
#' flag low-cardinality bitmap candidates.
#' @param bitmap_minimum_rows Minimum row count used by the bitmap screening
#' rule.
#' @param partition_minimum_rows Minimum row count used by the date partition
#' screening rule.
#' @param max_robust_sample Maximum deterministic sample size used to estimate
#' the medcouple statistic.
#' @param num_treads Optional positive integer thread cap for this call.
#'
#' @return A tibble with one row per selected column.
#'
#' @importFrom collapse fmean fmedian fquantile fsd fsum
#' @importFrom data.table as.data.table getDTthreads rbindlist
#' @importFrom data.table setDTthreads uniqueN
#' @importFrom robustbase mc
#' @importFrom stats pt qbeta qt
#' @importFrom stringi stri_length stri_numbytes stri_trans_nfkc
#' @importFrom stringr str_c str_detect str_squish str_to_lower
#' @importFrom stringr str_to_upper str_trim
#' @importFrom tibble as_tibble tibble
#' @importFrom utils head object.size
#' @export
sby_profile_data_catalog <- function(
  .data,
  ...,
  bitmap_cardinality_ratio = 0.01,
  bitmap_minimum_rows = 100000L,
  partition_minimum_rows = 1000000L,
  max_robust_sample = 100000L,
  num_treads = NULL
){
  sby_internal_validate_tabular_input(.data = .data)

  bitmapCardinalityRatio <- sby_internal_profile_validate_probability(
    value = bitmap_cardinality_ratio,
    argumentName = "bitmap_cardinality_ratio",
    allowBoundary = FALSE
  )
  bitmapMinimumRows <- sby_internal_profile_validate_positive_integer(
    value = bitmap_minimum_rows,
    argumentName = "bitmap_minimum_rows"
  )
  partitionMinimumRows <- sby_internal_profile_validate_positive_integer(
    value = partition_minimum_rows,
    argumentName = "partition_minimum_rows"
  )
  maxRobustSample <- sby_internal_profile_validate_positive_integer(
    value = max_robust_sample,
    argumentName = "max_robust_sample"
  )
  requestedThreads <- if(is.null(num_treads)){
    sby_internal_get_max_threads()
  } else {
    sby_internal_validate_max_threads(num_treads)
  }
  selectedColumns <- sby_internal_eval_select(
    .data = .data,
    ...,
    default = "all"
  )

  if(length(selectedColumns) == 0L){
    return(tibble())
  }

  selectedData <- .data[, unname(selectedColumns), drop = FALSE]

  sby_internal_with_thread_context(
    expr = sby_internal_profile_with_data_table_threads(
      expr = sby_internal_profile_data_catalog(
        .data = selectedData,
        bitmapCardinalityRatio = bitmapCardinalityRatio,
        bitmapMinimumRows = bitmapMinimumRows,
        partitionMinimumRows = partitionMinimumRows,
        maxRobustSample = maxRobustSample
      ),
      maxThreads = requestedThreads
    ),
    maxThreads = requestedThreads,
    useOpenmp = TRUE,
    useBlas = FALSE
  )
}

#' @title Profile Dataset-Level Structure
#'
#' @usage
#' sby_profile_data_set(
#'   .data,
#'   calculate_duplicate_rows = TRUE,
#'   num_treads = NULL
#' )
#'
#' @description
#' Summarize dataset dimensions, memory use, missing cells, complete rows, and
#' duplicate rows.
#'
#' @param .data A data frame or tibble.
#' @param calculate_duplicate_rows Logical scalar indicating whether exact
#' duplicate rows should be counted.
#' @param num_treads Optional positive integer thread cap for this call.
#'
#' @return A one-row tibble.
#'
#' @export
sby_profile_data_set <- function(
  .data,
  calculate_duplicate_rows = TRUE,
  num_treads = NULL
){
  sby_internal_validate_tabular_input(.data = .data)

  if(
    !is.logical(calculate_duplicate_rows) ||
    length(calculate_duplicate_rows) != 1L ||
    is.na(calculate_duplicate_rows)
  ){
    stop(
      "`calculate_duplicate_rows` must be a logical scalar",
      call. = FALSE
    )
  }

  requestedThreads <- if(is.null(num_treads)){
    sby_internal_get_max_threads()
  } else {
    sby_internal_validate_max_threads(num_treads)
  }

  sby_internal_with_thread_context(
    expr = sby_internal_profile_with_data_table_threads(
      expr = sby_internal_profile_data_set(
        .data = .data,
        calculateDuplicateRows = calculate_duplicate_rows
      ),
      maxThreads = requestedThreads
    ),
    maxThreads = requestedThreads,
    useOpenmp = TRUE,
    useBlas = FALSE
  )
}

#' @title Profile Missingness Patterns
#'
#' @usage
#' sby_profile_missing_patterns(
#'   .data,
#'   top_count = 20L,
#'   num_treads = NULL
#' )
#'
#' @description
#' Return the most frequent row-level combinations of missing columns.
#'
#' @param .data A data frame or tibble.
#' @param top_count Positive integer with the maximum number of patterns.
#' @param num_treads Optional positive integer thread cap for this call.
#'
#' @return A tibble ordered by decreasing pattern frequency.
#'
#' @export
sby_profile_missing_patterns <- function(
  .data,
  top_count = 20L,
  num_treads = NULL
){
  sby_internal_validate_tabular_input(.data = .data)

  topCount <- sby_internal_profile_validate_positive_integer(
    value = top_count,
    argumentName = "top_count"
  )
  requestedThreads <- if(is.null(num_treads)){
    sby_internal_get_max_threads()
  } else {
    sby_internal_validate_max_threads(num_treads)
  }

  sby_internal_with_thread_context(
    expr = sby_internal_profile_with_data_table_threads(
      expr = sby_internal_profile_missing_patterns(
        .data = .data,
        topCount = topCount
      ),
      maxThreads = requestedThreads
    ),
    maxThreads = requestedThreads,
    useOpenmp = TRUE,
    useBlas = FALSE
  )
}
