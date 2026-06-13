#' @title Executar expressão com contexto temporário de threads
#'
#' @usage
#' sby_internal_with_thread_context(
#'   expr,
#'   max_threads = NULL,
#'   use_openmp = TRUE,
#'   use_blas = TRUE
#' )
#'
#' @description
#' Avalia uma expressão R enquanto aplica temporariamente a política restrita de
#' controle de threads do pacote, com restauração garantida ao final.
#'
#' @details
#' A função usa \code{on.exit()} para restaurar o contexto mesmo quando a
#' expressão avaliada sinaliza erro. O controle de ambiente permanece limitado a
#' \code{OMP_THREAD_LIMIT} e \code{MKL_NUM_THREADS}, conforme a necessidade do
#' caminho computacional informado.
#'
#' @param expr Expressão R a ser avaliada sob o contexto temporário.
#'
#' @param max_threads Limite temporário de threads. Quando \code{NULL}, usa a
#' opção interna validada por \code{sby_internal_get_max_threads()}.
#'
#' @param use_openmp Booleano que autoriza o uso de \code{OMP_THREAD_LIMIT}.
#'
#' @param use_blas Booleano que autoriza o uso de \code{MKL_NUM_THREADS}.
#'
#' @return Resultado da expressão \code{expr}.
#'
#' @seealso \code{sby_internal_capture_thread_context()},
#' \code{sby_internal_apply_thread_context()},
#' \code{sby_internal_restore_thread_context()}
#'
#' @references
#' Wickham, H. \emph{Advanced R}. 2nd ed. Chapman and Hall/CRC, 2019.
#' Disponível em: \url{https://adv-r.hadley.nz/}.
#'
#' @examples
#' sby_internal_with_thread_context(1 + 1, max_threads = 2)
sby_internal_with_thread_context <- function(expr,
                                             max_threads = NULL,
                                             use_openmp = TRUE,
                                             use_blas = TRUE){

  # Obter o limite configurado quando o chamador nao informar valor explicito.
  if(is.null(max_threads)){
    max_threads <- sby_internal_get_max_threads()
  }

  # Capturar apenas as variaveis autorizadas para o caminho computacional.
  context <- sby_internal_capture_thread_context(
    use_openmp = use_openmp,
    use_blas = use_blas
  )

  # Garantir restauracao do contexto mesmo diante de erro na expressao.
  on.exit(sby_internal_restore_thread_context(context), add = TRUE)

  # Aplicar o limite temporario usando somente as variaveis permitidas.
  sby_internal_apply_thread_context(
    max_threads = max_threads,
    thread_context = context,
    use_openmp = use_openmp,
    use_blas = use_blas
  )

  # Avaliar a expressao prometida no ambiente do chamador.
  result <- force(expr)

  # Retornar o resultado original da expressao avaliada.
  return(result)
}
####
## End
#
