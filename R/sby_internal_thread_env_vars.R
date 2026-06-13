#' @title Listar variáveis de ambiente de controle de threads
#'
#' @usage sby_internal_thread_env_vars()
#'
#' @description
#' Retorna os nomes das únicas variáveis de ambiente que podem ser manipuladas
#' pelo controle interno de paralelismo do pacote.
#'
#' @details
#' A função centraliza uma política deliberadamente restritiva. Apenas
#' \code{OMP_THREAD_LIMIT} e \code{MKL_NUM_THREADS} são capturadas, alteradas e
#' restauradas. Outras variáveis de OpenMP, BLAS, OpenBLAS, BLIS, Accelerate,
#' NumExpr ou RcppParallel não são modificadas por este pacote.
#'
#' @section Política operacional:
#' \itemize{
#'   \item \code{OMP_THREAD_LIMIT} limita o número total de threads OpenMP.
#'   \item \code{MKL_NUM_THREADS} limita o número de threads usadas pela MKL.
#'   \item Nenhuma outra variável de ambiente é escrita por esta rotina.
#' }
#'
#' @return Vetor de caracteres com os nomes permitidos de variáveis de ambiente.
#'
#' @seealso \code{Sys.getenv()}, \code{Sys.setenv()},
#' \code{sby_internal_apply_thread_context()}
#'
#' @references
#' OpenMP Architecture Review Board. \emph{OpenMP Application Programming
#' Interface Specification}. Disponível em: \url{https://www.openmp.org/specifications/}.
#'
#' Intel. \emph{Intel oneAPI Math Kernel Library Developer Reference}.
#' Disponível em: \url{https://www.intel.com/content/www/us/en/docs/onemkl/developer-reference-c/}.
#'
#' @examples
#' sby_internal_thread_env_vars()
sby_internal_thread_env_vars <- function(){

  # Declarar explicitamente a lista permitida de variaveis manipulaveis.
  allowed_env_vars <- c("OMP_THREAD_LIMIT", "MKL_NUM_THREADS")

  # Retornar a lista para captura, aplicacao e restauracao do contexto.
  return(allowed_env_vars)
}
####
## End
#
