#' @title Inicializar opções padrão do pacote
#' @usage .onLoad(libname, pkgname)
#' @description
#' Define opções padrão de configuração quando o namespace do pacote é carregado.
#'
#' @details
#' A função é chamada automaticamente pelo R. Ela define thresholds de seleção de
#' backend e limite de threads somente quando as options correspondentes ainda
#' não existem. Não acessa banco de dados, não lê arquivos e não grava arquivos.
#' O retorno invisível preserva o contrato de hooks de carregamento.
#'
#' @param libname Caminho da biblioteca informado pelo R durante o carregamento.
#' @param pkgname Nome do pacote informado pelo R durante o carregamento.
#'
#' @return `NULL`, invisivelmente.
#'
#' @seealso sby_config
#' @keywords internal
.onLoad <- function(libname, pkgname){

  # Registra default runtime configuration values for automatic backends
  options(
    sby_config_start_fortran = getOption("sby_config_start_fortran", 10000L),
    sby_config_start_blas = getOption("sby_config_start_blas", 100000L),
    sby_config_max_threads = getOption("sby_config_max_threads", 2L)
  )

  # Retorna invisibly as required by .onLoad contract
  return(invisible(NULL))
}
####
## End
# 
