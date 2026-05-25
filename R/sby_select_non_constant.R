#' Remove constant columns efficiently
#'
#' `sby_select_non_constant()` removes selected columns whose values are
#' constant (all observations are equal)
#'
#' The native backend evaluates each column independently and uses implicit
#' OpenMP parallelism over columns when available
#'
#' @param .data A data.frame, tibble, or matrix
#' @param ... Tidyselect column selectors. If empty, all columns are evaluated
#'   Matrix input evaluates all columns
#'
#' @return `.data` without constant selected columns
#' @export
sby_select_non_constant <- function(.data, ...) {

  # Valida cedo para evitar custo desnecessario em entrada invalida
  sby_int_validate_tabular_input(.data)

  # Sai rapido quando nao ha linhas ou colunas
  if (ncol(.data) == 0L || nrow(.data) == 0L) {
    return(.data)
  }

  # Normaliza nomes para manter comportamento consistente no tidyselect
  resolved_names <- sby_int_resolve_column_names(.data)
  colnames(.data) <- resolved_names

  # Define quais colunas vao para o filtro
  selected <- sby_int_eval_select(.data, ..., default = "all")
  if (length(selected) == 0L) {
    return(.data)
  }

  # Converte para lista de vetores atomicos no formato esperado pelo backend
  selected_data <- .data[, unname(selected), drop = FALSE]
  selected_list <- as.list(as.data.frame(selected_data, stringsAsFactors = FALSE))

  # Recebe mascara logica do C onde TRUE mantem e FALSE remove
  keep_mask <- .Call("sby_non_constant_mask", selected_list, PACKAGE = "sbyops")
  removed <- colnames(selected_data)[!keep_mask]

  # Remove apenas colunas constantes dentro da selecao
  keep <- setdiff(colnames(.data), removed)
  out <- .data[, keep, drop = FALSE]

  # Preserva a classe original de saida
  return(sby_int_restore_selected_data(out, .data))
}
