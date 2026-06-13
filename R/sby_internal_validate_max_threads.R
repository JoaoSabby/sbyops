#' @title Validar limite máximo de threads
#'
#' @usage sby_internal_validate_max_threads(max_threads)
#'
#' @description
#' Verifica se o limite de threads informado é um escalar numérico positivo e
#' finito, adequado para parametrizar rotinas internas de paralelismo.
#'
#' @details
#' A validação impede valores vetoriais, ausentes, infinitos ou menores que um.
#' O retorno é convertido para inteiro para manter contrato estável com variáveis
#' de ambiente e chamadas nativas que esperam contagens discretas.
#'
#' @param max_threads Valor candidato ao número máximo de threads.
#'
#' @return Inteiro positivo com o limite validado de threads.
#'
#' @seealso \code{sby_config()}, \code{sby_internal_get_max_threads()}
#'
#' @references
#' R Core Team. \emph{R Extensions}. Vienna: R Foundation. Disponível em:
#' \url{https://cran.r-project.org/doc/manuals/r-release/R-exts.html}.
#'
#' @examples
#' sby_internal_validate_max_threads(2)
sby_internal_validate_max_threads <- function(max_threads){

  # Testar o contrato escalar, numerico, finito e positivo do argumento.
  invalid_threads <- !is.numeric(max_threads) ||
    length(max_threads) != 1L ||
    is.na(max_threads) ||
    !is.finite(max_threads) ||
    max_threads < 1

  # Interromper a execucao quando o limite nao respeitar o contrato publico.
  if(invalid_threads){
    stop("`sby_config_max_threads` must be a positive integer scalar", call. = FALSE)
  }

  # Converter para inteiro apos a validacao sem alterar a regra de dominio.
  validated_threads <- as.integer(max_threads)

  # Retornar o limite pronto para uso por variaveis de ambiente permitidas.
  return(validated_threads)
}
####
## End
#
