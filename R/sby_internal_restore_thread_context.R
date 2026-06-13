#' @title Restaurar contexto permitido de threads
#'
#' @usage sby_internal_restore_thread_context(thread_context)
#'
#' @description
#' Restaura as variáveis de ambiente autorizadas que foram capturadas antes da
#' aplicação temporária de limites internos de threads.
#'
#' @details
#' A restauração respeita a diferença entre variável ausente e variável definida
#' como cadeia vazia. Apenas os nomes presentes no contexto capturado são
#' restaurados. Como a captura é restrita, esta função também não manipula
#' variáveis fora de \code{OMP_THREAD_LIMIT} e \code{MKL_NUM_THREADS}.
#'
#' @param thread_context Lista produzida por
#' \code{sby_internal_capture_thread_context()}.
#'
#' @return \code{invisible(NULL)}, pois a restauração ocorre no ambiente do
#' processo R.
#'
#' @seealso \code{sby_internal_capture_thread_context()},
#' \code{sby_internal_apply_thread_context()}
#'
#' @references
#' R Core Team. \emph{R Language Definition}. Vienna: R Foundation. Disponível
#' em: \url{https://cran.r-project.org/doc/manuals/r-release/R-lang.html}.
#'
#' @examples
#' ctx <- sby_internal_capture_thread_context()
#' sby_internal_restore_thread_context(ctx)
sby_internal_restore_thread_context <- function(thread_context){

  # Obter os valores originais armazenados no contexto de entrada.
  env_values <- thread_context$env_vars

  # Obter a marcacao de ausencia original de cada variavel capturada.
  env_missing <- thread_context$env_missing

  # Percorrer somente as variaveis que foram capturadas pela politica permitida.
  for(i in seq_along(env_values)){

    # Identificar o nome da variavel autorizada na posicao corrente.
    key <- names(env_values)[i]

    # Remover a variavel quando ela estava ausente no momento da captura.
    if(isTRUE(env_missing[[i]])){
      Sys.unsetenv(key)
    } else {
      # Restaurar o valor original, incluindo cadeia vazia quando aplicavel.
      do.call(Sys.setenv, setNames(list(env_values[[i]]), key))
    }
  }

  # Retornar invisivelmente para evitar saida desnecessaria em on.exit().
  return(invisible(NULL))
}
####
## End
#
