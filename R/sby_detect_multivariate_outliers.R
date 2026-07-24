#' @title Detect Multivariate Outliers with Robust MCD
#'
#' @usage
#' sby_detect_multivariate_outliers(
#'   .data,
#'   ...,
#'   alpha = 0.001,
#'   max_fit_rows = 50000L,
#'   num_treads = NULL
#' )
#'
#' @description
#' Detecta observações atípicas multivariadas por distância de Mahalanobis
#' robusta estimada pelo determinante mínimo da covariância, ou MCD.
#'
#' @details
#' A rotina calcula, para as linhas completas e finitas, a estatística
#' \(D_i^2=(x_i-\hat\mu_R)^\top\hat\Sigma_R^{-1}(x_i-\hat\mu_R)\),
#' em que \(\hat\mu_R\) e \(\hat\Sigma_R\) são estimadores robustos de
#' localização e dispersão. O ponto de corte é o quantil superior da
#' distribuição \(\chi^2_p\), com \(p\) variáveis selecionadas. Linhas com
#' valores ausentes ou infinitos são preservadas no resultado, porém recebem
#' distância e indicador indefinidos.
#'
#' O ajuste é determinístico quando `max_fit_rows` impõe amostragem sistemática.
#' Isso evita custo cúbico desnecessário em bases muito altas e torna a rotina
#' reproduzível em pipelines. O estimador deve ser ajustado apenas em dados de
#' treino quando a saída for usada em modelagem preditiva, pois sua aplicação
#' prévia em todo o conjunto pode induzir vazamento de informação.
#'
#' @references
#' Rousseeuw, P. J. (1985). Multivariate estimation with high breakdown point.
#' In *Mathematical Statistics and Applications*. Reidel.
#'
#' Rousseeuw, P. J.; Van Driessen, K. (1999). A fast algorithm for the minimum
#' covariance determinant estimator. *Technometrics*, 41(3), 212--223.
#'
#' Maronna, R. A.; Martin, R. D.; Yohai, V. J.; Salibián-Barrera, M. (2019).
#' *Robust Statistics: Theory and Methods*. Wiley.
#'
#' @details
#' The estimator must be fitted only on training data when its distances or
#' flags are used by a predictive model. The function uses the sbyops BLAS
#' thread context, allowing the active oneMKL backend to respect `num_treads`.
#'
#' @param .data A data frame or tibble.
#' @param ... Tidyselect expressions. The default selects numeric columns.
#' @param alpha Upper-tail probability used for the chi-squared cutoff.
#' @param max_fit_rows Maximum deterministic sample size used to fit MCD.
#' @param num_treads Optional positive integer thread cap for this call.
#'
#' @return A tibble with one row per input row.
#'
#' @importFrom robustbase covMcd
#' @importFrom stats complete.cases mahalanobis qchisq sd
#' @export
sby_detect_multivariate_outliers <- function(
  .data,
  ...,
  alpha = 0.001,
  max_fit_rows = 50000L,
  num_treads = NULL
){
  # Select and validate numeric columns before matrix conversion because MCD
  # requires a finite multivariate numeric geometry with non-singular scale.
  sby_internal_validate_tabular_input(.data = .data)

  alphaValue <- sby_internal_profile_validate_probability(
    value = alpha,
    argumentName = "alpha",
    allowBoundary = FALSE
  )
  maxFitRows <- sby_internal_profile_validate_positive_integer(
    value = max_fit_rows,
    argumentName = "max_fit_rows"
  )
  requestedThreads <- if(is.null(num_treads)){
    sby_internal_get_max_threads()
  } else {
    sby_internal_validate_max_threads(num_treads)
  }
  selectedColumns <- sby_internal_eval_select(
    .data = .data,
    ...,
    default = "numeric"
  )

  if(length(selectedColumns) < 2L){
    stop(
      "At least two numeric columns must be selected",
      call. = FALSE
    )
  }

  selectedData <- .data[, unname(selectedColumns), drop = FALSE]
  sby_internal_validate_tabular_input(
    .data = selectedData,
    validate_column_types = TRUE
  )

  sby_internal_with_thread_context(
    expr = sby_internal_detect_multivariate_outliers(
      .data = selectedData,
      alphaValue = alphaValue,
      maxFitRows = maxFitRows
    ),
    maxThreads = requestedThreads,
    useOpenmp = TRUE,
    useBlas = TRUE
  )
}

sby_internal_detect_multivariate_outliers <- function(
  .data,
  alphaValue,
  maxFitRows
){
  numericMatrix <- data.matrix(.data)
  storage.mode(numericMatrix) <- "double"
  finiteByRow <- rowSums(!is.finite(numericMatrix)) == 0L
  completeFlags <- complete.cases(numericMatrix) & finiteByRow
  completeIndexes <- which(completeFlags)
  completeMatrix <- numericMatrix[
    completeFlags,
    ,
    drop = FALSE
  ]

  if(nrow(completeMatrix) <= ncol(completeMatrix) + 1L){
    stop(
      "There are not enough complete rows for MCD",
      call. = FALSE
    )
  }

  variableScales <- vapply(
    seq_len(ncol(completeMatrix)),
    function(columnIndex){
      sd(completeMatrix[, columnIndex])
    },
    numeric(1)
  )

  if(any(!is.finite(variableScales) | variableScales == 0)){
    invalidColumns <- colnames(completeMatrix)[
      !is.finite(variableScales) |
        variableScales == 0
    ]
    stop(
      str_c(
        "MCD requires non-constant columns. Review: ",
        str_c(invalidColumns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  fitIndexes <- if(nrow(completeMatrix) <= maxFitRows){
    seq_len(nrow(completeMatrix))
  } else {
    unique(
      as.integer(
        round(
          seq.int(
            from = 1,
            to = nrow(completeMatrix),
            length.out = maxFitRows
          )
        )
      )
    )
  }
  fitMatrix <- completeMatrix[fitIndexes, , drop = FALSE]
  robustCovariance <- covMcd(fitMatrix)
  robustDistances <- mahalanobis(
    completeMatrix,
    center = robustCovariance$center,
    cov = robustCovariance$cov
  )
  cutoffValue <- qchisq(
    1 - alphaValue,
    df = ncol(completeMatrix)
  )
  allDistances <- rep(NA_real_, nrow(numericMatrix))
  allFlags <- rep(NA, nrow(numericMatrix))
  allDistances[completeIndexes] <- robustDistances
  allFlags[completeIndexes] <- robustDistances > cutoffValue

  tibble(
    NUMERO_LINHA = seq_len(nrow(numericMatrix)),
    FLAG_LINHA_COMPLETA = completeFlags,
    DISTANCIA_MAHALANOBIS_ROBUSTA = allDistances,
    LIMITE_QUI_QUADRADO = cutoffValue,
    FLAG_OUTLIER_MCD = allFlags,
    QTD_VARIAVEIS = as.numeric(ncol(completeMatrix)),
    QTD_LINHAS_AJUSTE_MCD = as.numeric(nrow(fitMatrix))
  )
}
