#' Detectar colunas com baixa variabilidade por frequência modal
#'
#' @description
#' `sby_fe_filter_nzv()` identifica colunas em que um mesmo conteúdo aparece em proporção
#' maior ou igual a um limite informado por `threshold`.
#'
#' A sigla NZV vem de *near-zero variance*. Neste contexto, uma coluna é tratada
#' como pouco variável quando existe um valor dominante. O valor dominante pode
#' ser qualquer conteúdo da coluna: número, texto, fator, lógico ou `NA`.
#'
#' @details
#' ## Regra estatística
#'
#' Para cada coluna, a função calcula:
#'
#' \deqn{ratio = frequência\_do\_valor\_mais\_comum / número\_de\_linhas}
#'
#' A coluna entra no resultado quando:
#'
#' \deqn{ratio >= threshold}
#'
#' Exemplos:
#'
#' * `threshold = 1`: somente colunas completamente constantes entram.
#' * `threshold = 0.95`: entram colunas em que algum conteúdo representa pelo
#'   menos 95% das linhas.
#' * `threshold = 0`: toda coluna não vazia entra, porque toda coluna possui
#'   algum valor mais frequente com razão maior ou igual a zero.
#'
#' ## Tratamento de `NA`
#'
#' Valores ausentes são contabilizados como conteúdo. Portanto, se uma coluna tem
#' 95% de `NA`, o valor modal será `NA` e a coluna entrará quando
#' `threshold <= 0.95`.
#'
#' ## Tipos aceitos
#'
#' As colunas podem ser:
#'
#' * `factor`;
#' * `character`;
#' * `integer`;
#' * `logical`;
#' * `numeric`.
#'
#' Outros tipos são recusados com erro claro, porque a comparação de igualdade
#' pode não ser óbvia para listas, datas complexas, matrizes em coluna ou objetos
#' personalizados.
#'
#' ## Arquitetura R + Fortran
#'
#' A função R faz validações e transforma cada coluna em códigos inteiros
#' compactos. Conteúdos iguais recebem o mesmo código. Conteúdos diferentes
#' recebem códigos diferentes. `NA` também recebe um código.
#'
#' O Fortran recebe somente esses códigos. Isso torna o núcleo nativo mais
#' simples e rápido, porque a tarefa vira uma contagem direta de inteiros.
#'
#' O valor original é reconstruído no R após o retorno do Fortran.
#'
#' @param data Um `data.frame` ou `tibble`. Cada coluna deve ser `factor`,
#'   `character`, `integer`, `logical` ou `numeric`.
#' @param threshold Número escalar entre 0 e 1. Representa a frequência relativa
#'   mínima do valor modal para a coluna entrar no filtro.
#' @param n_threads Inteiro escalar opcional. Número de threads OpenMP usadas
#'   pelo núcleo Fortran. Quando `NULL`, a rotina não altera a configuração
#'   global do OpenMP e usa o padrão definido pelo ambiente, por exemplo
#'   `OMP_NUM_THREADS`. Para máximo desempenho seguro em servidores grandes,
#'   recomenda-se testar valores próximos ao número de núcleos físicos.
#'
#' @return
#' Um objeto da classe `sby_fe_filter_nzv_result`, baseado em `tibble` quando o pacote
#' `tibble` estiver disponível, ou em `data.frame` base caso contrário.
#'
#' A tabela retornada possui uma linha por coluna filtrada e contém:
#'
#' * `column`: nome da coluna filtrada;
#' * `ratio`: frequência relativa do valor dominante;
#' * `value`: valor dominante, preservado como representação textual para
#'   permitir mistura de tipos entre colunas;
#' * `count`: frequência absoluta do valor dominante.
#'
#' O número de linhas e de colunas da tabela analisada não é repetido em cada
#' linha do resultado. Esses valores ficam guardados em atributos do objeto:
#' `n_rows`, `n_cols` e `threshold`. O método de impressão mostra esses
#' metadados antes da tabela.
#'
#' Quando `data` não possui linhas ou colunas, a função emite mensagem e retorna
#' uma tabela vazia com a mesma estrutura de saída e com os metadados preenchidos.
#'
#'
#' @examples
#' df <- data.frame(
#'   a = c(0, 0, 0, 0, 1),
#'   b = c("x", "x", "x", "x", "y"),
#'   c = c(1, 2, 3, 4, 5),
#'   d = c(NA, NA, NA, NA, 10)
#' )
#'
#' sby_fe_filter_nzv(df, threshold = 0.8)
#'
#' # Em servidor com muitos núcleos:
#' sby_fe_filter_nzv(df, threshold = 0.95, n_threads = 48)
#'
#' @export
sby_fe_filter_nzv <- function(data, threshold, n_threads = NULL) {
  
  # A validação acontece no R porque mensagens de erro em R são mais amigáveis
  # do que mensagens vindas do código nativo.
  if (!inherits(data, "data.frame")) {
    stop("`data` deve ser um data.frame ou tibble.", call. = FALSE)
  }

  # O threshold é um escalar numérico fechado no intervalo [0, 1].
  # Valores fora desse intervalo tornam a interpretação estatística ambígua.
  if (missing(threshold)) {
    stop("`threshold` deve ser informado.", call. = FALSE)
  }

  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold)) {
    stop("`threshold` deve ser um número escalar não ausente.", call. = FALSE)
  }

  if (!is.finite(threshold)) {
    stop("`threshold` deve ser finito.", call. = FALSE)
  }

  if (threshold < 0 || threshold > 1) {
    stop("`threshold` deve estar entre 0 e 1.", call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # Validação de `n_threads`
  # ---------------------------------------------------------------------------
  # O argumento controla o número de threads OpenMP no núcleo nativo.
  #
  # `NULL` significa "não interferir": o OpenMP decide com base no ambiente.
  # Um inteiro positivo força o número de threads para esta chamada nativa.
  #
  # A validação fica no R para evitar comportamento indefinido no Fortran, como
  # receber número negativo, NA ou valor não inteiro.
  if (is.null(n_threads)) {
    n_threads_native <- 0L
  } else {
    if (!is.numeric(n_threads) || length(n_threads) != 1L ||
        is.na(n_threads) || !is.finite(n_threads)) {
      stop("`n_threads` deve ser NULL ou um número inteiro positivo.", call. = FALSE)
    }

    if (n_threads < 1 || n_threads != as.integer(n_threads)) {
      stop("`n_threads` deve ser NULL ou um número inteiro positivo.", call. = FALSE)
    }

    if (n_threads > .Machine$integer.max) {
      stop("`n_threads` é grande demais para ser enviado ao código nativo.", call. = FALSE)
    }

    n_threads_native <- as.integer(n_threads)
  }

  n_cols <- ncol(data)
  n_rows <- nrow(data)

  if (is.null(n_cols) || n_cols == 0L) {
    message("Tabela está vazia: nenhuma coluna foi encontrada.")
    return(sby_fe_filter_nzv_empty_result(n_rows = n_rows, n_cols = n_cols, threshold = threshold, n_threads = n_threads_native))
  }

  if (is.null(n_rows) || n_rows == 0L) {
    message("Tabela está vazia: nenhuma linha foi encontrada.")
    return(sby_fe_filter_nzv_empty_result(n_rows = n_rows, n_cols = n_cols, threshold = threshold, n_threads = n_threads_native))
  }

  # ---------------------------------------------------------------------------
  # Conferência dos tipos aceitos
  # ---------------------------------------------------------------------------
  # A função é deliberadamente restrita. Isso evita resultados surpreendentes
  # para objetos com semântica especial de comparação.
  ok <- vapply(
    data,
    function(x) {
      is.factor(x) || is.character(x) || is.integer(x) ||
        is.logical(x) || is.numeric(x)
    },
    logical(1)
  )

  if (!all(ok)) {
    bad <- names(data)[!ok]
    stop(
      "Todas as colunas devem ser factor, character, integer, logical ou numeric. ",
      "Colunas inválidas: ",
      paste(bad, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Codificação das colunas
  # ---------------------------------------------------------------------------
  # Cada coluna é convertida em códigos inteiros compactos.
  #
  # Exemplo:
  #   c("a", "b", "a", NA)
  #
  # vira:
  #   codes  = c(1, 2, 1, 3)
  #   levels = c("a", "b", NA)
  #
  # O Fortran trabalha apenas com `codes`. Após o retorno, `levels` permite
  # reconstruir o conteúdo modal.
  encoded <- lapply(data, sby_fe_filter_nzv_encode_column)

  codes <- lapply(encoded, `[[`, "codes")
  max_codes <- vapply(encoded, `[[`, integer(1), "max_code")
  levels <- lapply(encoded, `[[`, "levels")

  # ---------------------------------------------------------------------------
  # Chamada nativa
  # ---------------------------------------------------------------------------
  # O Fortran retorna índices de colunas, razões, códigos modais e contagens.
  # A reconstrução dos nomes e valores é feita abaixo.
  native <- .Call(
    "sby_fe_filter_nzv_codes_fortran",
    codes,
    as.integer(max_codes),
    as.numeric(threshold),
    n_threads_native
  )

  names(native) <- c("column_index", "ratio", "code", "count")

  if (length(native$column_index) == 0L) {
    return(sby_fe_filter_nzv_empty_result(n_rows = n_rows, n_cols = n_cols, threshold = threshold, n_threads = n_threads_native))
  }

  column_index <- native$column_index
  modal_code <- native$code

  column_names <- names(data)
  if (is.null(column_names)) {
    column_names <- paste0("V", seq_len(n_cols))
  }

  modal_values <- mapply(
    FUN = function(idx, code) {
      sby_fe_filter_nzv_format_value(levels[[idx]][code])
    },
    idx = column_index,
    code = modal_code,
    USE.NAMES = FALSE
  )

  out <- data.frame(
    column = column_names[column_index],
    ratio = native$ratio,
    value = unname(modal_values),
    count = native$count,
    stringsAsFactors = FALSE
  )

  # Ordenar por maior razão primeiro costuma ser mais útil para inspeção.
  ord <- order(out$ratio, decreasing = TRUE, out$column)
  out <- out[ord, , drop = FALSE]
  rownames(out) <- NULL

  sby_fe_filter_nzv_new_result(out, n_rows = n_rows, n_cols = n_cols, threshold = threshold, n_threads = n_threads_native)
}

#' Codificar uma coluna para uso pelo núcleo Fortran
#'
#' @description
#' Função auxiliar interna. Recebe uma coluna R e devolve códigos inteiros
#' compactos. `NA` é mantido como conteúdo por meio do argumento
#' `exclude = NULL` em `factor()`.
#'
#' @param x Vetor de uma coluna.
#'
#' @return Lista com `codes`, `levels` e `max_code`.
#'
#' @keywords internal
sby_fe_filter_nzv_encode_column <- function(x) {
  if (is.factor(x)) {
    # Para fator, `exclude = NULL` preserva NA como nível codificável.
    f <- factor(x, exclude = NULL)
    lev <- levels(f)
    codes <- as.integer(f)

    # Quando há NA como nível, `levels()` mostra NA real em versões modernas.
    # A saída textual final é resolvida por `sby_fe_filter_nzv_format_value()`.
    return(list(
      codes = codes,
      levels = lev,
      max_code = length(lev)
    ))
  }

  if (is.logical(x)) {
    # Lógicos têm domínio pequeno. A conversão por factor mantém TRUE/FALSE/NA.
    f <- factor(x, exclude = NULL)
    return(list(
      codes = as.integer(f),
      levels = levels(f),
      max_code = nlevels(f)
    ))
  }

  if (is.integer(x)) {
    # `factor()` é usado porque trata igualdade, compacta códigos e preserva NA.
    f <- factor(x, exclude = NULL)
    return(list(
      codes = as.integer(f),
      levels = levels(f),
      max_code = nlevels(f)
    ))
  }

  if (is.numeric(x)) {
    # Números são codificados por igualdade exata, não por arredondamento.
    # Isso é intencional: a função mede repetição literal do conteúdo armazenado.
    f <- factor(x, exclude = NULL)
    return(list(
      codes = as.integer(f),
      levels = levels(f),
      max_code = nlevels(f)
    ))
  }

  if (is.character(x)) {
    # Caracteres são codificados no R para evitar manipulação de CHARSXP no
    # núcleo Fortran. Isso também preserva corretamente strings com acentos.
    f <- factor(x, exclude = NULL)
    return(list(
      codes = as.integer(f),
      levels = levels(f),
      max_code = nlevels(f)
    ))
  }

  stop("Tipo de coluna não suportado.", call. = FALSE)
}

#' Formatar valor modal para a tabela de saída
#'
#' @description
#' Função auxiliar interna. A saída usa representação textual porque cada linha
#' do resultado pode se referir a uma coluna de tipo diferente. Um único vetor R
#' não consegue misturar, de forma atômica, números, textos, lógicos e fatores
#' preservando todos os tipos originais.
#'
#' @param x Valor recuperado dos níveis codificados.
#'
#' @return String de tamanho 1.
#'
#' @keywords internal
sby_fe_filter_nzv_format_value <- function(x) {
  if (length(x) == 0L || is.na(x)) {
    return(NA_character_)
  }

  as.character(x)
}

#' Criar resultado vazio padronizado
#'
#' @description
#' Função auxiliar interna para garantir que todos os retornos vazios tenham a
#' mesma estrutura e os mesmos metadados.
#'
#' @param n_rows Número de linhas da tabela analisada.
#' @param n_cols Número de colunas da tabela analisada.
#' @param threshold Limite usado na análise.
#'
#' @return Objeto `sby_fe_filter_nzv_result` vazio.
#'
#' @keywords internal
sby_fe_filter_nzv_empty_result <- function(n_rows = 0L, n_cols = 0L, threshold = NA_real_, n_threads = NA_integer_) {
  out <- data.frame(
    column = character(),
    ratio = numeric(),
    value = character(),
    count = integer(),
    stringsAsFactors = FALSE
  )

  sby_fe_filter_nzv_new_result(out, n_rows = n_rows, n_cols = n_cols, threshold = threshold, n_threads = n_threads)
}

#' Construir objeto de resultado da análise NZV
#'
#' @description
#' Função auxiliar interna. Converte a tabela de saída para `tibble`, quando
#' disponível, e adiciona atributos com metadados globais da análise.
#'
#' Os atributos evitam repetir em cada linha informações que pertencem ao objeto
#' inteiro, como o número de linhas da base, o número de colunas analisadas e o
#' threshold utilizado.
#'
#' @param x Data frame com as colunas `column`, `ratio`, `value` e `count`.
#' @param n_rows Número de linhas da tabela analisada.
#' @param n_cols Número de colunas da tabela analisada.
#' @param threshold Limite usado na análise.
#' @param n_threads Número de threads solicitado. Valor zero indica que o
#'   ambiente OpenMP decidiu a configuração.
#'
#' @return Objeto da classe `sby_fe_filter_nzv_result`.
#'
#' @keywords internal
sby_fe_filter_nzv_new_result <- function(x, n_rows, n_cols, threshold, n_threads = NA_integer_) {
  if (requireNamespace("tibble", quietly = TRUE)) {
    x <- tibble::as_tibble(x)
  }

  attr(x, "n_rows") <- as.integer(n_rows)
  attr(x, "n_cols") <- as.integer(n_cols)
  attr(x, "threshold") <- as.numeric(threshold)
  attr(x, "n_threads") <- as.integer(n_threads)

  class(x) <- unique(c("sby_fe_filter_nzv_result", class(x)))
  x
}

#' Imprimir resultado de `sby_fe_filter_nzv()`
#'
#' @description
#' Método S3 responsável por exibir, antes da tabela, os metadados globais da
#' análise. O objetivo é mostrar informações úteis sem repetir esses valores em
#' todas as linhas do resultado.
#'
#' @param x Objeto retornado por `sby_fe_filter_nzv()`.
#' @param ... Argumentos repassados para o próximo método de impressão.
#'
#' @return O próprio objeto `x`, invisivelmente.
#'
#' @export
print.sby_fe_filter_nzv_result <- function(x, ...) {
  n_rows <- attr(x, "n_rows", exact = TRUE)
  n_cols <- attr(x, "n_cols", exact = TRUE)
  threshold <- attr(x, "threshold", exact = TRUE)
  n_threads <- attr(x, "n_threads", exact = TRUE)

  if (is.null(n_rows)) n_rows <- NA_integer_
  if (is.null(n_cols)) n_cols <- NA_integer_
  if (is.null(threshold)) threshold <- NA_real_
  if (is.null(n_threads)) n_threads <- NA_integer_

  cat("<sby_fe_filter_nzv_result>\n")
  cat("Linhas analisadas: ", format(n_rows, big.mark = ".", decimal.mark = ","), "\n", sep = "")
  cat("Colunas analisadas: ", format(n_cols, big.mark = ".", decimal.mark = ","), "\n", sep = "")
  cat("Threshold: ", format(threshold, decimal.mark = ","), "\n", sep = "")
  if (!is.na(n_threads) && n_threads > 0L) {
    cat("Threads OpenMP solicitadas: ", format(n_threads, big.mark = ".", decimal.mark = ","), "\n", sep = "")
  } else {
    cat("Threads OpenMP solicitadas: padrão do ambiente\n")
  }
  cat("Colunas filtradas: ", format(nrow(x), big.mark = ".", decimal.mark = ","), "\n\n", sep = "")

  NextMethod()
  invisible(x)
}
####
## Fim
#