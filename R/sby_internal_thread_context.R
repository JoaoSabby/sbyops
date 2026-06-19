#' @title Listar variáveis de ambiente de paralelismo
#' @usage sby_internal_thread_env_vars()
#' @description
#' Retorna os nomes das variáveis de ambiente que podem limitar OpenMP, BLAS,
#' MKL, OpenBLAS, BLIS, Accelerate, NumExpr e RcppParallel durante rotinas
#' internas do pacote.
#'
#' @details
#' A função apenas monta um vetor de caracteres. Ela não lê nem altera o
#' ambiente do processo. O vetor retornado é usado por rotinas internas que
#' capturam e restauram contexto de threads. O uso direto é arriscado porque a
#' lista representa o contrato interno atual do pacote.
#'
#' @return Vetor de caracteres com nomes de variáveis de ambiente.
#'
#' @seealso sby_internal_capture_thread_context
#' @keywords internal
sby_internal_thread_env_vars <- function(){
  c(
    "OMP_NUM_THREADS",
    "OMP_THREAD_LIMIT",
    "OMP_DYNAMIC",
    "OMP_PROC_BIND",
    "OMP_PLACES",
    "OMP_MAX_ACTIVE_LEVELS",
    "MKL_NUM_THREADS",
    "MKL_DYNAMIC",
    "MKL_DOMAIN_NUM_THREADS",
    "OPENBLAS_NUM_THREADS",
    "GOTO_NUM_THREADS",
    "BLIS_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS",
    "NUMEXPR_NUM_THREADS",
    "RCPP_PARALLEL_NUM_THREADS"
  )
}

#' @title Validar limite máximo de threads
#' @usage sby_internal_validate_max_threads(maxThreads)
#' @description
#' Valida se o limite de threads informado é um escalar numérico positivo e o
#' converte para inteiro.
#'
#' @details
#' A função existe para centralizar a validação da option
#' `sby_config_max_threads` e dos argumentos internos que controlam contexto de
#' threads. Ela gera erro quando o valor é ausente, não finito, não numérico,
#' vetorial ou menor que um. Não altera options nem variáveis de ambiente.
#'
#' @param maxThreads Escalar numérico que representa o limite desejado.
#'
#' @return Inteiro positivo de comprimento um.
#'
#' @seealso sby_config
#' @keywords internal
sby_internal_validate_max_threads <- function(maxThreads){
  if(!is.numeric(maxThreads) || length(maxThreads) != 1L || is.na(maxThreads) ||
     !is.finite(maxThreads) || maxThreads < 1){
    stop("`sby_config_max_threads` must be a positive integer scalar", call. = FALSE)
  }
  as.integer(maxThreads)
}

#' @title Obter limite máximo de threads configurado
#' @usage sby_internal_get_max_threads()
#' @description
#' Lê a option `sby_config_max_threads` e retorna o limite validado para uso em
#' rotinas internas.
#'
#' @details
#' A option é lida no momento da chamada. Quando ausente, o valor de fallback é
#' `2L`. Valores inválidos produzem erro pela validação compartilhada. A função
#' não altera estado global.
#'
#' @return Inteiro positivo com o limite de threads.
#'
#' @seealso sby_config, sby_internal_validate_max_threads
#' @keywords internal
sby_internal_get_max_threads <- function(){
  sby_internal_validate_max_threads(getOption("sby_config_max_threads", 2L))
}

#' @title Obter funções opcionais do pacote RhpcBLASctl
#' @usage sby_internal_get_rhpcblasctl()
#' @description
#' Localiza, quando disponível, funções de leitura e ajuste de threads BLAS e
#' OpenMP fornecidas por RhpcBLASctl.
#'
#' @details
#' A função usa `requireNamespace` sem anexar o pacote. Quando RhpcBLASctl não
#' está instalado, retorna `NULL`. Quando instalado, retorna uma lista com
#' funções existentes ou `NULL` em cada componente ausente. Não altera estado
#' global.
#'
#' @return `NULL` ou lista com componentes `blasGet`, `blasSet`, `ompGet` e
#' `ompSet`.
#'
#' @seealso sby_internal_capture_thread_context
#' @keywords internal
sby_internal_get_rhpcblasctl <- function(){
  if(!requireNamespace("RhpcBLASctl", quietly = TRUE)) return(NULL)
  ns <- getNamespace("RhpcBLASctl")
  list(
    blasGet = if(exists("blas_get_num_procs", envir = ns, mode = "function")) get("blas_get_num_procs", envir = ns) else NULL,
    blasSet = if(exists("blas_set_num_threads", envir = ns, mode = "function")) get("blas_set_num_threads", envir = ns) else NULL,
    ompGet  = if(exists("omp_get_max_threads", envir = ns, mode = "function")) get("omp_get_max_threads", envir = ns) else NULL,
    ompSet  = if(exists("omp_set_num_threads", envir = ns, mode = "function")) get("omp_set_num_threads", envir = ns) else NULL
  )
}

#' @title Capturar contexto atual de threads
#' @usage sby_internal_capture_thread_context(useOpenmp = TRUE, useBlas = TRUE)
#' @description
#' Captura variáveis de ambiente, options e estado opcional de RhpcBLASctl antes
#' de alterações temporárias de paralelismo.
#'
#' @details
#' A função preserva valores de ambiente, flags de ausência, options `mc.cores`
#' e `Ncpus`, além de threads BLAS e OpenMP quando RhpcBLASctl está disponível.
#' Ela não modifica o ambiente. O retorno é uma lista interna que deve ser
#' repassada para restauração. É segura para pipelines desde que o contexto seja
#' restaurado no mesmo processo.
#'
#' @param useOpenmp Lógico que indica se o estado OpenMP deve ser consultado.
#' @param useBlas Lógico que indica se o estado BLAS deve ser consultado.
#'
#' @return Lista com variáveis, options e metadados de restauração.
#'
#' @seealso sby_internal_restore_thread_context
#' @keywords internal
sby_internal_capture_thread_context <- function(useOpenmp = TRUE, useBlas = TRUE){
  envVars <- sby_internal_thread_env_vars()
  current <- Sys.getenv(envVars, unset = NA_character_)
  context <- list(
    envVars       = current,
    envMissing    = is.na(current),
    optionsValues = options("mc.cores", "Ncpus"),
    rhpc          = NULL
  )

  rhpc <- sby_internal_get_rhpcblasctl()
  if(!is.null(rhpc)){
    rhpcState <- list(used = TRUE, blasThreads = NULL, ompThreads = NULL, canRestoreBlas = FALSE, canRestoreOmp = FALSE)
    if(useBlas && !is.null(rhpc$blasGet) && !is.null(rhpc$blasSet)){
      val <- suppressWarnings(tryCatch(as.integer(rhpc$blasGet()), error = function(e) NULL))
      if(!is.null(val) && length(val) == 1L && !is.na(val) && val >= 1L){
        rhpcState$blasThreads    <- val
        rhpcState$canRestoreBlas <- TRUE
      }
    }
    if(useOpenmp && !is.null(rhpc$ompGet) && !is.null(rhpc$ompSet)){
      val <- suppressWarnings(tryCatch(as.integer(rhpc$ompGet()), error = function(e) NULL))
      if(!is.null(val) && length(val) == 1L && !is.na(val) && val >= 1L){
        rhpcState$ompThreads    <- val
        rhpcState$canRestoreOmp <- TRUE
      }
    }
    context$rhpc <- c(rhpcState, rhpc)
  }

  context
}

#' @title Aplicar contexto temporário de threads
#' @usage
#' sby_internal_apply_thread_context(
#'   maxThreads,
#'   threadContext,
#'   useOpenmp = TRUE,
#'   useBlas = TRUE
#' )
#' @description
#' Ajusta variáveis de ambiente, options e RhpcBLASctl para limitar threads em
#' etapas internas.
#'
#' @details
#' A função modifica estado global do processo por meio de `Sys.setenv`,
#' `options` e, quando disponível, funções de RhpcBLASctl. O argumento
#' `threadContext` deve ter sido produzido pela captura interna, embora não seja
#' validado estruturalmente. A chamada não restaura valores por si só.
#'
#' @param maxThreads Escalar numérico positivo com o limite desejado.
#' @param threadContext Lista de contexto capturada previamente.
#' @param useOpenmp Lógico que controla aplicação de variáveis e API OpenMP.
#' @param useBlas Lógico que controla aplicação de variáveis e API BLAS.
#'
#' @return `NULL`, de forma invisível pelo efeito das chamadas de ambiente.
#'
#' @seealso sby_internal_capture_thread_context, sby_internal_restore_thread_context
#' @keywords internal
sby_internal_apply_thread_context <- function(maxThreads, threadContext, useOpenmp = TRUE, useBlas = TRUE){
  maxThreads <- sby_internal_validate_max_threads(maxThreads)
  envUpdate <- c(
    OMP_NUM_THREADS           = as.character(maxThreads),
    OMP_THREAD_LIMIT          = as.character(maxThreads),
    OMP_DYNAMIC               = "FALSE",
    OMP_MAX_ACTIVE_LEVELS     = "1",
    MKL_NUM_THREADS           = as.character(maxThreads),
    MKL_DYNAMIC               = "FALSE",
    OPENBLAS_NUM_THREADS      = as.character(maxThreads),
    GOTO_NUM_THREADS          = as.character(maxThreads),
    BLIS_NUM_THREADS          = as.character(maxThreads),
    VECLIB_MAXIMUM_THREADS    = as.character(maxThreads),
    NUMEXPR_NUM_THREADS       = as.character(maxThreads),
    RCPP_PARALLEL_NUM_THREADS = as.character(maxThreads)
  )

  if(!useOpenmp){
    envUpdate <- envUpdate[setdiff(names(envUpdate), c("OMP_NUM_THREADS", "OMP_THREAD_LIMIT", "OMP_DYNAMIC", "OMP_MAX_ACTIVE_LEVELS"))]
  }
  if(!useBlas){
    envUpdate <- envUpdate[setdiff(names(envUpdate), c("MKL_NUM_THREADS", "MKL_DYNAMIC", "OPENBLAS_NUM_THREADS", "GOTO_NUM_THREADS", "BLIS_NUM_THREADS", "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"))]
  }

  do.call(Sys.setenv, as.list(envUpdate))
  options(mc.cores = maxThreads, Ncpus = maxThreads)

  if(!is.null(threadContext$rhpc) && isTRUE(threadContext$rhpc$used)){
    if(useBlas   && !is.null(threadContext$rhpc$blasSet)) suppressWarnings(try(threadContext$rhpc$blasSet(maxThreads), silent = TRUE))
    if(useOpenmp && !is.null(threadContext$rhpc$ompSet))  suppressWarnings(try(threadContext$rhpc$ompSet(maxThreads),  silent = TRUE))
  }
}

#' @title Restaurar contexto de threads
#' @usage sby_internal_restore_thread_context(threadContext)
#' @description
#' Restaura variáveis de ambiente, options e estado BLAS ou OpenMP capturados
#' anteriormente.
#'
#' @details
#' A função altera estado global do processo para retornar ao contexto salvo. Ela
#' desdefine variáveis originalmente ausentes, restaura options `mc.cores` e
#' `Ncpus`, e tenta restaurar RhpcBLASctl quando havia capacidade de leitura e
#' escrita na captura. Erros dessas tentativas são suprimidos.
#'
#' @param threadContext Lista produzida por `sby_internal_capture_thread_context`.
#'
#' @return `NULL`, de forma invisível pelo efeito das chamadas de restauração.
#'
#' @seealso sby_internal_capture_thread_context
#' @keywords internal
sby_internal_restore_thread_context <- function(threadContext){
  envValues  <- threadContext$envVars
  envMissing <- threadContext$envMissing
  for(i in seq_along(envValues)){
    key <- names(envValues)[i]
    if(envMissing[[i]]) Sys.unsetenv(key) else do.call(Sys.setenv, setNames(list(envValues[[i]]), key))
  }

  oldMc    <- threadContext$optionsValues$mc.cores
  oldNcpus <- threadContext$optionsValues$Ncpus
  if(is.null(oldMc))    options(mc.cores = NULL) else options(mc.cores = oldMc)
  if(is.null(oldNcpus)) options(Ncpus    = NULL) else options(Ncpus    = oldNcpus)

  if(!is.null(threadContext$rhpc) && isTRUE(threadContext$rhpc$used)){
    if(isTRUE(threadContext$rhpc$canRestoreBlas) && !is.null(threadContext$rhpc$blasSet)) suppressWarnings(try(threadContext$rhpc$blasSet(threadContext$rhpc$blasThreads), silent = TRUE))
    if(isTRUE(threadContext$rhpc$canRestoreOmp)  && !is.null(threadContext$rhpc$ompSet))  suppressWarnings(try(threadContext$rhpc$ompSet(threadContext$rhpc$ompThreads),  silent = TRUE))
  }
}

#' @title Executar expressão com limite temporário de threads
#' @usage
#' sby_internal_with_thread_context(
#'   expr,
#'   maxThreads = NULL,
#'   useOpenmp = TRUE,
#'   useBlas = TRUE
#' )
#' @description
#' Avalia uma expressão depois de aplicar limite temporário de threads e restaura
#' o contexto ao sair.
#'
#' @details
#' A função usa `on.exit` para restaurar o contexto capturado. Quando
#' `maxThreads` é `NULL`, lê `sby_config_max_threads` com fallback `2L`. A
#' expressão é forçada no ambiente da chamada. O uso direto deve considerar que a
#' função modifica estado global durante a avaliação.
#'
#' @param expr Expressão R a ser avaliada.
#' @param maxThreads Escalar numérico positivo ou `NULL`.
#' @param useOpenmp Lógico que controla limitação de OpenMP.
#' @param useBlas Lógico que controla limitação de BLAS.
#'
#' @return Valor retornado por `expr`.
#'
#' @seealso sby_config, sby_internal_apply_thread_context
#' @keywords internal
sby_internal_with_thread_context <- function(expr, maxThreads = NULL, useOpenmp = TRUE, useBlas = TRUE){
  if(is.null(maxThreads)) maxThreads <- sby_internal_get_max_threads()
  context <- sby_internal_capture_thread_context(useOpenmp = useOpenmp, useBlas = useBlas)
  on.exit(sby_internal_restore_thread_context(context), add = TRUE)
  sby_internal_apply_thread_context(maxThreads = maxThreads, threadContext = context, useOpenmp = useOpenmp, useBlas = useBlas)
  force(expr)
}
