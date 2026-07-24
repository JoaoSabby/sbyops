#' @title Configurar limiares e threads do sbyops
#' @name sby_config
#'
#' @usage
#' sby_config(
#'   sby_config_start_fortran = 10000L,
#'   sby_config_start_blas = 100000L,
#'   sby_config_max_threads = 2L
#' )
#'
#' @description
#' Define opções globais de planejamento de execução usadas pelas ferramentas do
#' pacote para escolher backends e limitar paralelismo.
#'
#' @details
#' A configuração é armazenada em `options()` e lida no momento de cada chamada.
#' Os limiares `sby_config_start_fortran` e `sby_config_start_blas` controlam a
#' troca automática de estratégias em rotinas que possuem múltiplos backends. O
#' valor `sby_config_max_threads` representa o limite padrão de threads para
#' funções que executam um contexto temporário de OpenMP, BLAS ou `data.table`.
#'
#' @param sby_config_start_fortran Inteiro positivo com o limiar de ativação do
#' backend Fortran em estratégias automáticas.
#' @param sby_config_start_blas Inteiro positivo com o limiar de ativação do
#' backend BLAS em estratégias automáticas.
#' @param sby_config_max_threads Inteiro positivo com o limite global padrão de
#' threads.
#'
#' @return Lista nomeada com os valores de configuração validados e gravados.
#'
#' @examples
#' sby_config(sby_config_max_threads = 2L)
#' @export
sby_config <- function(sby_config_start_fortran = 10000L,
                       sby_config_start_blas = 100000L,
                       sby_config_max_threads = 2L){

  # Validate Fortran threshold as positive integer scalar
  if(!is.numeric(sby_config_start_fortran) || length(sby_config_start_fortran) != 1L ||
     !is.finite(sby_config_start_fortran) || sby_config_start_fortran < 1L){
    stop("`sby_config_start_fortran` must be a positive integer scalar", call. = FALSE)
  }

  # Validate BLAS threshold as positive integer scalar
  if(!is.numeric(sby_config_start_blas) || length(sby_config_start_blas) != 1L ||
     !is.finite(sby_config_start_blas) || sby_config_start_blas < 1L){
    stop("`sby_config_start_blas` must be a positive integer scalar", call. = FALSE)
  }


  sby_config_max_threads <- sby_internal_validate_max_threads(sby_config_max_threads)

  options(
    sby_config_start_fortran = as.integer(sby_config_start_fortran),
    sby_config_start_blas = as.integer(sby_config_start_blas),
    sby_config_max_threads = sby_config_max_threads
  )

  # Build configuration payload for return visibility
  configuration <- list(
    sby_config_start_fortran = getOption("sby_config_start_fortran"),
    sby_config_start_blas = getOption("sby_config_start_blas"),
    sby_config_max_threads = getOption("sby_config_max_threads")
  )

  # Return current sbyops configuration values
  return(configuration)
}
####
## End
# 
