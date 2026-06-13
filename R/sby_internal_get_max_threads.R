#' @title Obter limite máximo configurado de threads
#'
#' @usage sby_internal_get_max_threads()
#'
#' @description
#' Lê a opção interna \code{sby_config_max_threads} e aplica a validação formal
#' usada pelas rotinas computacionais do pacote.
#'
#' @details
#' Quando a opção não está definida, o valor conservador \code{2L} é utilizado.
#' A validação é delegada a \code{sby_internal_validate_max_threads()} para
#' preservar uma única regra de consistência.
#'
#' @return Inteiro positivo com o limite máximo de threads configurado.
#'
#' @seealso \code{sby_config()}, \code{sby_internal_validate_max_threads()}
#'
#' @references
#' R Core Team. \emph{R Internals}. Vienna: R Foundation. Disponível em:
#' \url{https://cran.r-project.org/doc/manuals/r-release/R-ints.html}.
#'
#' @examples
#' sby_internal_get_max_threads()
sby_internal_get_max_threads <- function(){

  # Ler a opcao interna com padrao conservador para maquinas compartilhadas.
  configured_threads <- getOption("sby_config_max_threads", 2L)

  # Validar e normalizar o valor lido da opcao interna do pacote.
  max_threads <- sby_internal_validate_max_threads(configured_threads)

  # Retornar o limite que sera usado por rotinas de execucao pesada.
  return(max_threads)
}
####
## End
#
