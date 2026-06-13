#' @title Capturar contexto permitido de threads
#'
#' @usage sby_internal_capture_thread_context(use_openmp = TRUE, use_blas = TRUE)
#'
#' @description
#' Captura o estado atual das variáveis de ambiente autorizadas para controle
#' temporário de threads durante rotinas internas de alto custo computacional.
#'
#' @details
#' A captura é restrita às variáveis retornadas por
#' \code{sby_internal_thread_env_vars()}. O argumento \code{use_openmp} decide se
#' \code{OMP_THREAD_LIMIT} participa do contexto. O argumento \code{use_blas}
#' decide se \code{MKL_NUM_THREADS} participa do contexto. Nenhuma outra variável
#' de ambiente, opção de R ou API externa é capturada ou manipulada.
#'
#' @param use_openmp Booleano que indica se \code{OMP_THREAD_LIMIT} será
#' capturada para possível restauração posterior.
#'
#' @param use_blas Booleano que indica se \code{MKL_NUM_THREADS} será capturada
#' para possível restauração posterior.
#'
#' @return Lista com valores originais e indicação de variáveis ausentes.
#'
#' @seealso \code{sby_internal_thread_env_vars()},
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
#' sby_internal_capture_thread_context()
sby_internal_capture_thread_context <- function(use_openmp = TRUE, use_blas = TRUE){

  # Iniciar a lista de variaveis autorizadas pela politica interna.
  env_vars <- sby_internal_thread_env_vars()

  # Remover a variavel OpenMP quando a chamada nao usa caminho OpenMP pesado.
  if(!isTRUE(use_openmp)){
    env_vars <- setdiff(env_vars, "OMP_THREAD_LIMIT")
  }

  # Remover a variavel MKL quando a chamada nao usa caminho BLAS/MKL pesado.
  if(!isTRUE(use_blas)){
    env_vars <- setdiff(env_vars, "MKL_NUM_THREADS")
  }

  # Capturar valores atuais preservando a diferenca entre ausente e vazio.
  current_values <- Sys.getenv(env_vars, unset = NA_character_)

  # Montar o contexto minimo necessario para restauracao posterior segura.
  thread_context <- list(
    env_vars = current_values,
    env_missing = is.na(current_values)
  )

  # Retornar o contexto sem consultar ou alterar APIs externas de threads.
  return(thread_context)
}
####
## End
#
