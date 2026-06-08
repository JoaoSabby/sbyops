#' @title Select Columns by Modal Frequency
#'
#' @usage
#' sby_select_modal_frequency(
#'   .data,
#'   threshold
#' )
#'
#' @description Remove selected columns whose most frequent value proportion is greater than or equal to a threshold
#'
#' @details Uses a native type-specialized backend for logical, integer/factor, numeric, and character columns.
#'
#' @param .data A data frame or tibble
#'
#' @param threshold A numeric scalar in the closed interval `[0, 1]`
#'
#' @return An object with the same structural class as `.data` with high modal-frequency columns removed
#' @export
#' Filtra colunas com base na frequencia modal
#'
#' @param .data dados
#' @param threshold limite de frequencia
#'
#' @returns
#' @export
#'
#' @examples
sby_select_modal_frequency <- function(.data, threshold = 0.99){

  # Resultado
  colResult <- lapply(
    X = .data,
    FUN = function(col, size_records){countOccur(col)[1, 2] / size_records},
    size_records = fnrow(.data)
  )

  # Converte em vetor
  colResult <- unlist(colResult)

  # Remove columns
  result <- .data |>
    fselect(names(colResult[colResult < 0.99]))

  # Retorna
  return(result)
}
####
## End
#
