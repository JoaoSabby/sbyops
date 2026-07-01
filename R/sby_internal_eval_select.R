#' @title Internal Helper: Eval Select
#'
#' @usage sby_internal_eval_select(.data, ..., default = c("all", "numeric"))
#'
#' @description Resolve selected columns from tidyselect expressions or default selection policy
#'
#' @details This helper enforces matrix selector constraints and computes deterministic fallback selections for data-frame-like inputs when selectors are absent
#'
#' @section Internal contract:
#' This function should be invoked by validated selector entry points
#'
#' @param .data A data frame, tibble, or matrix
#'
#' @param ... Tidyselect expressions
#'
#' @param default Default policy used when selectors are omitted
#'
#' @return An integer index vector with names for selected columns
#'
#' @seealso [sby_select_correlation()], [sby_select_modal_frequency()], [sby_select_non_constant()]
#'
#' @references R CORE TEAM. Writing R Extensions. Vienna: R Foundation. Available at: \href{https://cran.r-project.org/doc/manuals/r-release/R-exts.html}{R Extensions Manual}
#'
#' @examples
#' exampleData <- data.frame(a = 1:3, b = letters[1:3])
#' sby_internal_eval_select(exampleData, default = "all")
sby_internal_eval_select <- function(.data, ..., default = c("all", "numeric")){

  # Resolve fallback mode and capture selectors lazily for tidy evaluation
  default_mode <- match.arg(default)
  selector_expressions <- rlang::enquos(...)

  # Enforce matrix constraint where tidyselect expressions are invalid
  if(is.matrix(.data)){
    if(length(selector_expressions) > 0L){
      stop("Tidyselect columns are not supported for matrix inputs.", call. = FALSE)
    }

    selected_indexes <- seq_len(ncol(.data))
    return(selected_indexes)
  }

  # Resolve default selections when explicit selectors are not provided
  if(length(selector_expressions) == 0L){
    if(default_mode == "numeric"){
      numeric_mask <- vapply(
        X = .data,
        FUN = function(column_data){
          is.null(dim(column_data)) && (is.numeric(column_data) || is.logical(column_data))
        },
        FUN.VALUE = logical(1L)
      )
      selected_indexes <- seq_along(numeric_mask)[numeric_mask]
    } else {
      selected_indexes <- seq_along(.data)
    }

    names(selected_indexes) <- names(.data)[selected_indexes]

    # Return selected column indexes with stable names
    return(selected_indexes)
  }

  # Evaluate user-provided selectors against data columns. External character
  # vectors may be injected with `!!` by callers and can contain bookkeeping
  # columns that are not present in the current data. Normalize those injected
  # vectors before the tidyselect call so both positive and negative selections
  # such as `!!vars` and `-!!vars` keep their tidyselect semantics. Ordinary
  # tidyselect expressions remain strict because only explicit `!!` character
  # injections are rewritten.
  repaired_selectors <- lapply(
    selector_expressions,
    sby_internal_prune_injected_character_selector,
    column_names = names(.data)
  )
  selection_expression <- rlang::expr(c(!!!repaired_selectors))
  selected_indexes <- tidyselect::eval_select(selection_expression, .data)

  # Return explicit selection indexes for downstream slicing
  return(selected_indexes)
}

#' @title Internal Helper: Prune Injected Character Selectors
#'
#' @description Limit injected character vectors to columns present in data
#'
#' @param selector A quosure captured from tidyselect dots
#'
#' @param column_names Existing column names
#'
#' @return A quosure with injected character vectors pruned
sby_internal_prune_injected_character_selector <- function(selector, column_names){
  selector_expression <- rlang::quo_get_expr(selector)
  repaired_expression <- sby_internal_prune_character_expression(
    selector_expression,
    column_names = column_names,
    selector_env = rlang::quo_get_env(selector)
  )

  rlang::quo_set_expr(selector, repaired_expression)
}

#' @title Internal Helper: Prune Character Expression
#'
#' @description Recursively remove missing names from literal character vectors
#'
#' @param expression A language object or atomic vector
#'
#' @param column_names Existing column names
#'
#' @param selector_env Environment where injected variables are resolved
#'
#' @return A repaired expression object
sby_internal_prune_character_expression <- function(expression, column_names, selector_env){
  if(is.character(expression)){
    return(intersect(expression, column_names))
  }

  if(!is.call(expression)){
    return(expression)
  }

  if(sby_internal_is_unquote_expression(expression)){
    injected_value <- rlang::eval_tidy(expression[[2L]][[2L]], env = selector_env)
    if(is.character(injected_value)){
      return(intersect(injected_value, column_names))
    }
    return(injected_value)
  }

  repaired_call <- as.list(expression)
  if(length(repaired_call) > 1L){
    repaired_call[-1L] <- lapply(
      repaired_call[-1L],
      sby_internal_prune_character_expression,
      column_names = column_names,
      selector_env = selector_env
    )
  }

  as.call(repaired_call)
}

#' @title Internal Helper: Detect Unquote Expressions
#'
#' @description Detect parser-level `!!` calls that remain inside a captured selector
#'
#' @param expression A language object
#'
#' @return TRUE when expression is a nested `!` call representing `!!`
sby_internal_is_unquote_expression <- function(expression){
  is.call(expression) &&
    identical(expression[[1L]], as.name("!")) &&
    length(expression) == 2L &&
    is.call(expression[[2L]]) &&
    identical(expression[[2L]][[1L]], as.name("!")) &&
    length(expression[[2L]]) == 2L
}
####
## End
# 
