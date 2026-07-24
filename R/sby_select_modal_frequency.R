#' @title Selecionar colunas por frequência modal
#'
#' @usage
#' sby_select_modal_frequency(.data, ..., threshold = 0.99)
#'
#' @description
#' Remove colunas cuja categoria ou valor mais frequente representa proporção
#' maior ou igual ao limiar informado.
#'
#' @details
#' A ferramenta identifica variáveis praticamente constantes, mas ainda não
#' estritamente constantes, por meio da frequência do valor modal. Colunas fora
#' da seleção `tidyselect` são sempre preservadas. A implementação usa contagem
#' vetorizada em R para manter fallback previsível e evitar dependência de
#' símbolos nativos em ambientes incompletos.
#'
#' @param .data Data frame, tibble ou matriz.
#' @param ... Expressões tidyselect. Quando omitidas, todas as colunas são
#' avaliadas.
#' @param threshold Escalar numérico no intervalo fechado `[0, 1]`.
#'
#' @return Objeto com a mesma classe estrutural de `.data`, sem as colunas de
#' alta frequência modal selecionadas.
#' @seealso [sby_select_non_constant()], [sby_select_correlation()]
#' @export
sby_select_modal_frequency <- function(.data, ..., threshold = 0.99){

  # Validate supported tabular input and threshold before scanning columns
  sby_internal_validate_tabular_input(.data = .data)
  threshold <- sby_internal_validate_threshold_scalar(
    threshold = threshold,
    arg_name = "threshold"
  )

  # Cache dimensions once because they are reused by validation and filtering
  n_cols <- collapse::fncol(.data)
  n_rows <- collapse::fnrow(.data)

  # Return unchanged input for degenerate tabular shapes
  if(n_cols == 0L || n_rows == 0L){
    return(.data)
  }

  # Normalize names for deterministic column removal and restoration
  resolved_names <- sby_internal_resolve_column_names(.data = .data)
  colnames(.data) <- resolved_names

  # Resolve selected columns using the all-columns default policy
  selected_columns <- sby_internal_eval_select(
    .data = .data,
    ...,
    default = "all"
  )

  # Return unchanged input when no columns are selected for evaluation
  if(length(selected_columns) == 0L){
    return(.data)
  }

  # Materialize selected columns once before computing modal frequencies
  selected_data <- .data[, unname(selected_columns), drop = FALSE]
  sby_internal_validate_tabular_input(
    .data = selected_data,
    validate_column_types = TRUE
  )
  column_data <- as.data.frame(selected_data, stringsAsFactors = FALSE)
  cutoff <- ceiling(threshold * n_rows)
  count_occur <- kit::countOccur

  # Compute the keep mask entirely in R so execution never depends on native symbol availability
  keep_mask <- vapply(
    X = column_data,
    FUN = function(current_column){
      occurrence_table <- count_occur(current_column)

      if(nrow(occurrence_table) == 0L){
        return(TRUE)
      }

      max_count <- max(as.numeric(occurrence_table[, 2L]), na.rm = TRUE)
      max_count < cutoff
    },
    FUN.VALUE = logical(1L)
  )

  # Remove only selected columns whose modal proportion reaches or exceeds the threshold
  removed_columns <- names(column_data)[!keep_mask]
  kept_columns <- setdiff(colnames(.data), removed_columns)
  filtered_data <- .data[, kept_columns, drop = FALSE]

  # Restore output class to match the original input structure
  return(
    sby_internal_restore_selected_data(
      selected_data = filtered_data,
      original = .data
    )
  )
}
####
## End
#
