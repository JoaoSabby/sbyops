#' @title Aplicar contexto temporário permitido de threads
#'
#' @usage
#' sby_internal_apply_thread_context(
#'   max_threads,
#'   thread_context,
#'   use_openmp = TRUE,
#'   use_blas = TRUE
#' )
#'
#' @description
#' Define temporariamente apenas as variáveis de ambiente autorizadas para
#' limitar threads em caminhos computacionais internos selecionados.
#'
#' @details
#' A função nunca altera variáveis de ambiente fora da lista permitida nem
#' opções globais de R. A escrita é limitada a \code{OMP_THREAD_LIMIT}, quando
#' \code{use_openmp} é verdadeiro, e a \code{MKL_NUM_THREADS}, quando
#' \code{use_blas} é verdadeiro.
#'
#' @param max_threads Escalar numérico positivo com o limite temporário de
#' threads.
#'
#' @param thread_context Contexto capturado previamente. O argumento é mantido
#' no contrato para explicitar o par captura/aplicação/restauração.
#'
#' @param use_openmp Booleano que autoriza escrever \code{OMP_THREAD_LIMIT}.
#'
#' @param use_blas Booleano que autoriza escrever \code{MKL_NUM_THREADS}.
#'
#' @return \code{invisible(NULL)}, pois o efeito pretendido é temporário e
#' externo ao objeto de retorno.
#'
#' @seealso \code{sby_internal_capture_thread_context()},
#' \code{sby_internal_restore_thread_context()}
#'
#' @references
#' OpenMP Architecture Review Board. \emph{OpenMP Application Programming
#' Interface Specification}. Disponível em: \url{https://www.openmp.org/specifications/}.
#'
#' Intel. \emph{Intel oneAPI Math Kernel Library Developer Reference}.
#' Disponível em: \url{https://www.intel.com/content/www/us/en/docs/onemkl/developer-reference-c/}.
#'
#' @examples
#' ctx <- sby_internal_capture_thread_context()
#' sby_internal_apply_thread_context(2, ctx)
#' sby_internal_restore_thread_context(ctx)
sby_internal_apply_thread_context <- function(max_threads,
                                              thread_context,
                                              use_openmp = TRUE,
                                              use_blas = TRUE){

  # Validar o limite antes de escrever qualquer variavel de ambiente.
  max_threads <- sby_internal_validate_max_threads(max_threads)

  # Preparar a lista inicialmente vazia para evitar escrita nao autorizada.
  env_update <- character(0L)

  # Incluir somente OMP_THREAD_LIMIT quando o caminho OpenMP for permitido.
  if(isTRUE(use_openmp)){
    env_update["OMP_THREAD_LIMIT"] <- as.character(max_threads)
  }

  # Incluir somente MKL_NUM_THREADS quando o caminho BLAS/MKL for permitido.
  if(isTRUE(use_blas)){
    env_update["MKL_NUM_THREADS"] <- as.character(max_threads)
  }

  # Aplicar as variaveis permitidas somente quando houver algo a definir.
  if(length(env_update) > 0L){
    do.call(Sys.setenv, as.list(env_update))
  }

  # Validar minimamente o contrato estrutural do contexto recebido.
  if(!is.list(thread_context)){
    stop("`thread_context` must be a list", call. = FALSE)
  }

  # Retornar invisivelmente para uso seguro em chamadas internas.
  return(invisible(NULL))
}
####
## End
#
