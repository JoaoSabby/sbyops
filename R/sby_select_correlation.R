#' @title Selecionar colunas por correlação de Pearson
#'
#' @description
#' Remove colunas numéricas redundantes quando a correlação absoluta de Pearson
#' atinge ou excede um limiar definido pelo usuário.
#'
#' @details
#' A função seleciona colunas numéricas por `tidyselect`, calcula dependência
#' linear par a par e remove, em cada conflito, a variável com maior correlação
#' média absoluta contra as demais candidatas ativas. A estratégia de execução é
#' escolhida automaticamente entre rotinas de streaming, Fortran e BLAS, de
#' acordo com os limiares configurados por [sby_config()].
#'
#' @param .data Data frame, tibble, data.table ou matriz numericamente compatível.
#' @param ... Expressões tidyselect. Quando omitidas, colunas numéricas são
#' avaliadas.
#' @param threshold Escalar numérico em `[0, 1]` usado como limiar de remoção.
#' @param num_treads Inteiro positivo opcional com limite temporário de threads.
#'
#' @return Objeto tabular com colunas correlacionadas removidas.
#' @seealso [sby_select_non_constant()], [sby_select_modal_frequency()]
#' @importFrom cli cli_alert_info
#' @export
sby_select_correlation <- function(.data, ..., threshold, num_treads = NULL){

  sby_internal_validate_tabular_input(.data = .data)
  threshold <- sby_internal_validate_correlation_threshold(threshold = threshold)

  selected_columns <- sby_internal_eval_select(.data = .data, ..., default = "numeric")

  selected_data <- sby_internal_subset_columns(.data, unname(selected_columns))
  sby_internal_validate_tabular_input(
    .data = selected_data,
    validate_column_types = TRUE
  )

  numeric_matrix <- data.matrix(selected_data)
  storage.mode(numeric_matrix) <- "double"

  selected_strategy <- sby_internal_select_correlation_strategy(selected_data = numeric_matrix)

  requested_threads <- if(is.null(num_treads)){
    sby_internal_get_max_threads()
  } else {
    sby_internal_validate_max_threads(num_treads)
  }
  context <- sby_internal_capture_thread_context(
    useOpenmp = selected_strategy == "fortran",
    useBlas   = selected_strategy == "fortran"
  )
  on.exit(sby_internal_restore_thread_context(context), add = TRUE)

  if(selected_strategy == "fortran"){
    sby_internal_apply_thread_context(
      maxThreads    = requested_threads,
      threadContext = context,
      useOpenmp     = TRUE,
      useBlas       = TRUE
    )

    removed_columns <- sby_internal_compute_correlation_fortran(
      numeric_matrix = numeric_matrix,
      threshold      = threshold
    )
  } else {
    removed_columns <- sby_internal_apply_correlation_selection(
      cor_mat   = sby_internal_compute_correlation_streaming(mat = numeric_matrix),
      threshold = threshold
    )
  }

  sby_internal_restore_selected_data(
    selected_data = sby_internal_subset_columns(.data, setdiff(colnames(.data), removed_columns)),
    original = .data
  )
}
