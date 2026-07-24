#' @title Remover colunas constantes
#' @name sby_select_non_constant
#'
#' @usage sby_select_non_constant(.data, ...)
#'
#' @description
#' Remove colunas selecionadas que contêm um único valor repetido ao longo de
#' todas as observações.
#'
#' @details
#' A ferramenta é uma etapa inicial de limpeza para reduzir variáveis sem
#' informação amostral. O backend nativo em C é usado quando disponível; caso
#' contrário, uma implementação R segura é aplicada. Colunas fora da seleção são
#' preservadas.
#'
#' @param .data Data frame, tibble, data.table ou matriz.
#' @param ... Expressões tidyselect. Quando omitidas, todas as colunas são
#' avaliadas.
#'
#' @return Objeto com a mesma classe estrutural de `.data`, sem as colunas
#' constantes selecionadas.
#'
#' @seealso [sby_select_correlation()], [sby_select_modal_frequency()]
#'
#' @examples
#' constant_data <- data.frame(a = c(1, 1, 1), b = c(1, 2, 3))
#' sby_select_non_constant(constant_data)
#' @export
sby_select_non_constant <- function(.data, ...){

  sby_internal_validate_tabular_input(.data = .data)

  if(collapse::fncol(.data) == 0L || collapse::fnrow(.data) == 0L){
    return(.data)
  }

  if(inherits(.data, "data.table") && requireNamespace("data.table", quietly = TRUE)){
    .data <- data.table::copy(.data)
  }

  resolved_names <- sby_internal_resolve_column_names(.data = .data)
  colnames(.data) <- resolved_names

  selected_columns <- sby_internal_eval_select(
    .data = .data,
    ...,
    default = "all"
  )
  if(length(selected_columns) == 0L){
    return(.data)
  }

  selected_data <- sby_internal_subset_columns(.data, unname(selected_columns))
  sby_internal_validate_tabular_input(
    .data = selected_data,
    validate_column_types = TRUE
  )
  selected_list <- as.list(as.data.frame(selected_data, stringsAsFactors = FALSE))

  keep_mask <- sby_internal_non_constant_mask(selected_list)
  removed_columns <- colnames(selected_data)[!keep_mask]
  kept_columns <- setdiff(colnames(.data), removed_columns)
  filtered_data <- sby_internal_subset_columns(.data, kept_columns)

  sby_internal_restore_selected_data(
    selected_data = filtered_data,
    original = .data
  )
}

#' @title Compute Non-Constant Column Mask
#' @name sby_internal_non_constant_mask
#'
#' @description Compute a logical mask indicating which selected columns are not constant
#'
#' @param cols A list of selected columns
#'
#' @return A logical vector with TRUE for non-constant columns
sby_internal_non_constant_mask <- function(cols){
  if(!is.list(cols)){
    stop("`cols` must be a list.", call. = FALSE)
  }

  if(is.loaded("sby_internal_non_constant_mask", PACKAGE = "sbyops")){
    return(.Call(
      "sby_internal_non_constant_mask",
      cols,
      PACKAGE = "sbyops"
    ))
  }

  vapply(cols, sby_internal_is_non_constant_column, logical(1))
}

#' @title Check Whether a Column Is Non-Constant
#' @name sby_internal_is_non_constant_column
#'
#' @description Pure-R fallback used when the native non-constant backend is unavailable
#'
#' @param x A selected column
#'
#' @return TRUE when the column contains at least two distinct values
sby_internal_is_non_constant_column <- function(x){
  if(!is.atomic(x) || !is.null(dim(x))){
    return(TRUE)
  }

  n <- length(x)
  if(n <= 1L){
    return(FALSE)
  }

  first <- x[[1L]]
  first_missing <- is.na(first)
  for(i in seq.int(2L, n)){
    current <- x[[i]]
    if(first_missing){
      if(!is.na(current)){
        return(TRUE)
      }
    } else if(is.na(current) || current != first){
      return(TRUE)
    }
  }

  FALSE
}
####
## End
# 
