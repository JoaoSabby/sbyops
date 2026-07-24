#' @title Listar distribuições contínuas candidatas
#'
#' @description
#' Retorna o catálogo curado de distribuições contínuas candidatas usado por
#' `sby_identify_distribution()`, incluindo domínio de suporte e indicação de
#' famílias mistas.
#'
#' @details
#' O catálogo organiza famílias GAMLSS por suporte: real, positivo e intervalos
#' unitários com ou sem massa nos limites. A tabela é deliberadamente explícita
#' para permitir auditoria estatística do conjunto de modelos avaliado.
#'
#' @return Um tibble com nome da distribuição, domínio e indicador de família
#' mista.
#'
#' @export
sby_distribution_candidates <- function(){
  # Keep candidate metadata as plain vectors to avoid runtime dependency on
  # external GAMLSS objects before the fitting function explicitly needs them.
  candidateNames <- c(
    "NO",
    "GU",
    "RG",
    "LO",
    "NET",
    "TF",
    "PE",
    "SN1",
    "SN2",
    "exGAUS",
    "SHASH",
    "SHASHo",
    "EGB2",
    "JSU",
    "SEP1",
    "SEP2",
    "SEP3",
    "SEP4",
    "ST1",
    "ST2",
    "ST3",
    "ST4",
    "ST5",
    "SST",
    "GT",
    "EXP",
    "GA",
    "IG",
    "LOGNO",
    "WEI",
    "WEI2",
    "WEI3",
    "IGAMMA",
    "PARETO2",
    "GP",
    "BCCG",
    "GG",
    "GIG",
    "LNO",
    "BCT",
    "BCPE",
    "GB2",
    "BE",
    "BEo",
    "BEINF0",
    "BEINF1",
    "BEOI",
    "BEZI",
    "BEINF",
    "GB1"
  )
  candidateDomains <- c(
    rep("REAL", 25),
    rep("POSITIVO", 17),
    "UNITARIO_ABERTO",
    "UNITARIO_ABERTO",
    "UNITARIO_ZERO",
    "UNITARIO_UM",
    "UNITARIO_FECHADO",
    "UNITARIO_ZERO",
    "UNITARIO_FECHADO",
    "UNITARIO_ABERTO"
  )
  mixedFlags <- candidateNames %in%
    c(
      "BEINF0",
      "BEINF1",
      "BEOI",
      "BEZI",
      "BEINF"
    )

  tibble(
    DISTRIBUICAO = candidateNames,
    DOMINIO = candidateDomains,
    FLAG_DISTRIBUICAO_MISTA = mixedFlags
  )
}

#' @title Identificar melhor distribuição suportada
#'
#' @usage
#' sby_identify_distribution(
#'   x,
#'   candidates = sby_distribution_candidates(),
#'   max_sample_size = 100000L,
#'   tail_probability = 0.01,
#'   model_prior = NULL,
#'   num_treads = NULL
#' )
#'
#' @description
#' Identifica distribuições contínuas candidatas para um vetor numérico por
#' ajuste de máxima verossimilhança, critérios de informação e diagnósticos
#' empíricos de aderência.
#'
#' @details
#' Para cada família compatível com o suporte observado, a rotina ajusta um
#' modelo `gamlss`, calcula \(\ell(\hat\theta)\), AIC, AICc, BIC, pesos de
#' Akaike e probabilidades posteriores aproximadas via BIC. A aproximação
#' bayesiana é condicional ao conjunto de modelos, aos ajustes convergentes e
#' aos pesos `model_prior`; portanto, não deve ser interpretada como prova de
#' que uma distribuição verdadeira pertence ao catálogo.
#'
#' As estatísticas de Kolmogorov--Smirnov, Cramér--von Mises e
#' Anderson--Darling são reportadas como medidas de ranqueamento, sem valores-p
#' ingênuos, porque os parâmetros são estimados nos mesmos dados. Amostras muito
#' longas são reduzidas por amostragem determinística para preservar velocidade,
#' reprodutibilidade e consumo previsível de memória.
#'
#' @references
#' Akaike, H. (1974). A new look at the statistical model identification.
#' *IEEE Transactions on Automatic Control*, 19(6), 716--723.
#'
#' Schwarz, G. (1978). Estimating the dimension of a model. *The Annals of
#' Statistics*, 6(2), 461--464.
#'
#' Rigby, R. A.; Stasinopoulos, D. M. (2005). Generalized additive models for
#' location, scale and shape. *Applied Statistics*, 54(3), 507--554.
#'
#' Burnham, K. P.; Anderson, D. R. (2002). *Model Selection and Multimodel
#' Inference*. Springer.
#'
#' Os pacotes GAMLSS são dependências opcionais em tempo de execução. A
#' função gera erro claro quando eles não estão instalados.
#'
#' @param x Vetor numérico finito após remoção de ausências e infinitos.
#' @param candidates Tabela candidata retornada por
#' `sby_distribution_candidates()` ou estrutura compatível.
#' @param max_sample_size Inteiro positivo ou `Inf`. Vetores maiores são
#' amostrados de forma determinística.
#' @param tail_probability Probabilidade usada para sinalizar outliers de cauda.
#' @param model_prior Vetor numérico nomeado opcional com pesos prévios
#' positivos para modelos.
#' @param num_treads Inteiro positivo opcional com limite temporário de threads.
#'
#' @return Tibble com uma linha por candidata avaliada.
#'
#' @export
sby_identify_distribution <- function(
  x,
  candidates = sby_distribution_candidates(),
  max_sample_size = 100000L,
  tail_probability = 0.01,
  model_prior = NULL,
  num_treads = NULL
){
  if(
    !is.numeric(x) ||
    inherits(x, "Date") ||
    inherits(x, "POSIXt")
  ){
    stop(
      "`x` must be numeric and cannot inherit from a date class",
      call. = FALSE
    )
  }

  if(
    !inherits(candidates, "data.frame") ||
    !all(
      c(
        "DISTRIBUICAO",
        "DOMINIO",
        "FLAG_DISTRIBUICAO_MISTA"
      ) %in% names(candidates)
    )
  ){
    stop("`candidates` has an invalid structure", call. = FALSE)
  }

  finiteValues <- suppressWarnings(
    as.numeric(x[is.finite(x)])
  )

  if(length(finiteValues) < 20L){
    stop(
      "At least 20 finite observations are required",
      call. = FALSE
    )
  }

  if(uniqueN(finiteValues) < 3L){
    stop(
      "At least three distinct values are required",
      call. = FALSE
    )
  }

  candidateData <- as.data.frame(
    candidates,
    stringsAsFactors = FALSE
  )
  validDomains <- c(
    "REAL",
    "POSITIVO",
    "UNITARIO_ABERTO",
    "UNITARIO_ZERO",
    "UNITARIO_UM",
    "UNITARIO_FECHADO"
  )

  if(
    nrow(candidateData) == 0L ||
    !is.character(candidateData$DISTRIBUICAO) ||
    !is.character(candidateData$DOMINIO) ||
    anyNA(candidateData$DISTRIBUICAO) ||
    anyNA(candidateData$DOMINIO) ||
    anyNA(candidateData$FLAG_DISTRIBUICAO_MISTA) ||
    any(candidateData$DISTRIBUICAO == "") ||
    anyDuplicated(candidateData$DISTRIBUICAO) > 0L ||
    any(!candidateData$DOMINIO %in% validDomains) ||
    !is.logical(candidateData$FLAG_DISTRIBUICAO_MISTA)
  ){
    stop("`candidates` contains invalid values", call. = FALSE)
  }

  if(
    !is.null(model_prior) &&
    (
      !is.numeric(model_prior) ||
        is.null(names(model_prior)) ||
        anyNA(names(model_prior)) ||
        any(names(model_prior) == "") ||
        anyDuplicated(names(model_prior)) > 0L ||
        any(!is.finite(model_prior)) ||
        any(model_prior <= 0)
    )
  ){
    stop(
      "`model_prior` must be a positive named numeric vector",
      call. = FALSE
    )
  }

  maxSampleSize <- if(
    is.numeric(max_sample_size) &&
    length(max_sample_size) == 1L &&
    is.infinite(max_sample_size) &&
    max_sample_size > 0
  ){
    Inf
  } else {
    sby_internal_profile_validate_positive_integer(
      value = max_sample_size,
      argumentName = "max_sample_size"
    )
  }
  tailProbability <- sby_internal_profile_validate_probability(
    value = tail_probability,
    argumentName = "tail_probability",
    allowBoundary = FALSE
  )
  requestedThreads <- if(is.null(num_treads)){
    sby_internal_get_max_threads()
  } else {
    sby_internal_validate_max_threads(num_treads)
  }
  distributionRuntime <- sby_internal_distribution_runtime()

  sby_internal_with_thread_context(
    expr = sby_internal_profile_with_data_table_threads(
      expr = sby_internal_identify_distribution(
        x = finiteValues,
        candidates = candidates,
        maxSampleSize = maxSampleSize,
        tailProbability = tailProbability,
        modelPrior = model_prior,
        distributionRuntime = distributionRuntime
      ),
      maxThreads = requestedThreads
    ),
    maxThreads = requestedThreads,
    useOpenmp = TRUE,
    useBlas = TRUE
  )
}

sby_internal_distribution_runtime <- function(){
  requiredPackages <- c("gamlss", "gamlss.dist")
  missingPackages <- requiredPackages[
    !vapply(
      requiredPackages,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ]

  if(length(missingPackages) > 0L){
    stop(
      str_c(
        "Missing optional packages: ",
        str_c(missingPackages, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  gamlssNamespace <- getNamespace("gamlss")
  distributionNamespace <- getNamespace("gamlss.dist")

  list(
    gamlssMl = get(
      "gamlssML",
      envir = gamlssNamespace,
      mode = "function"
    ),
    distributionNamespace = distributionNamespace
  )
}

sby_internal_distribution_support <- function(values, domainName){
  minimumValue <- min(values)
  maximumValue <- max(values)

  switch(
    domainName,
    REAL = TRUE,
    POSITIVO = minimumValue > 0,
    UNITARIO_ABERTO = minimumValue > 0 && maximumValue < 1,
    UNITARIO_ZERO = minimumValue >= 0 && maximumValue < 1,
    UNITARIO_UM = minimumValue > 0 && maximumValue <= 1,
    UNITARIO_FECHADO = minimumValue >= 0 && maximumValue <= 1,
    FALSE
  )
}

sby_internal_distribution_goodness <- function(
  sortedValues,
  model,
  distributionName,
  tailProbability,
  distributionNamespace
){
  observationCount <- length(sortedValues)
  cdfFunction <- get(
    str_c("p", distributionName),
    envir = distributionNamespace,
    mode = "function"
  )
  parameterValues <- lapply(
    model$parameters,
    function(parameterName) model[[parameterName]]
  )
  names(parameterValues) <- model$parameters
  cdfValues <- do.call(
    cdfFunction,
    c(
      list(q = sortedValues),
      parameterValues
    )
  )

  if(
    length(cdfValues) != observationCount ||
    any(!is.finite(cdfValues))
  ){
    stop("Fitted CDF returned invalid values", call. = FALSE)
  }

  epsilonValue <- .Machine$double.eps
  cdfValues <- pmin(
    pmax(cdfValues, epsilonValue),
    1 - epsilonValue
  )
  sequenceValues <- seq_len(observationCount)
  empiricalUpper <- sequenceValues / observationCount
  empiricalLower <- (sequenceValues - 1) / observationCount
  expectedPositions <- (
    2 * sequenceValues - 1
  ) / (
    2 * observationCount
  )
  ksStatistic <- max(
    abs(cdfValues - empiricalUpper),
    abs(cdfValues - empiricalLower)
  )
  cvmStatistic <- 1 / (12 * observationCount) +
    sum((cdfValues - expectedPositions) ^ 2)
  adStatistic <- -observationCount -
    mean(
      (2 * sequenceValues - 1) *
        (
          log(cdfValues) +
            log(1 - rev(cdfValues))
        )
    )
  tailFlags <- cdfValues < tailProbability / 2 |
    cdfValues > 1 - tailProbability / 2

  list(
    KS_STAT = ksStatistic,
    CVM_STAT = cvmStatistic,
    AD_STAT = adStatistic,
    QTD_OUTLIER_CAUDA = as.numeric(sum(tailFlags)),
    PERC_OUTLIER_CAUDA = mean(tailFlags)
  )
}

sby_internal_fit_distribution <- function(
  values,
  distributionName,
  domainName,
  mixedFlag,
  tailProbability,
  distributionRuntime
){
  supportCompatible <- sby_internal_distribution_support(
    values = values,
    domainName = domainName
  )
  observationCount <- length(values)
  result <- list(
    DISTRIBUICAO = distributionName,
    DOMINIO = domainName,
    FLAG_DISTRIBUICAO_MISTA = mixedFlag,
    SUPORTE_COMPATIVEL = supportCompatible,
    CONVERGIU = FALSE,
    FLAG_CONVERGENCIA_REPORTADA = NA,
    CODIGO_CONVERGENCIA = NA_character_,
    QTD_PARAMETROS = NA_real_,
    LOG_VEROSSIMILHANCA = NA_real_,
    AIC = NA_real_,
    AICC = NA_real_,
    BIC = NA_real_,
    KS_STAT = NA_real_,
    CVM_STAT = NA_real_,
    AD_STAT = NA_real_,
    QTD_OUTLIER_CAUDA = NA_real_,
    PERC_OUTLIER_CAUDA = NA_real_,
    PARAMETROS = NA_character_,
    AVISO = NA_character_,
    ERRO = NA_character_
  )

  if(!supportCompatible){
    result$ERRO <- "SUPORTE INCOMPATIVEL"
    return(result)
  }

  warningMessages <- character()
  fittedModel <- tryCatch(
    withCallingHandlers(
      {
        familyFunction <- get(
          distributionName,
          envir = distributionRuntime$distributionNamespace,
          mode = "function"
        )
        familyObject <- familyFunction()
        distributionRuntime$gamlssMl(
          values,
          family = familyObject
        )
      },
      warning = function(warningCondition){
        warningMessages <<- c(
          warningMessages,
          conditionMessage(warningCondition)
        )
        invokeRestart("muffleWarning")
      }
    ),
    error = function(errorCondition) errorCondition
  )

  if(inherits(fittedModel, "error")){
    result$ERRO <- conditionMessage(fittedModel)
    result$AVISO <- if(length(warningMessages) > 0L){
      str_c(unique(warningMessages), collapse = "; ")
    } else {
      NA_character_
    }
    return(result)
  }

  parameterCount <- as.numeric(fittedModel$df.fit)
  globalDeviance <- as.numeric(fittedModel$G.deviance)
  logLikelihood <- -0.5 * globalDeviance
  aicValue <- as.numeric(fittedModel$aic)
  bicValue <- as.numeric(fittedModel$sbc)
  aiccValue <- if(
    observationCount - parameterCount - 1 > 0
  ){
    aicValue +
      2 * parameterCount * (parameterCount + 1) /
      (observationCount - parameterCount - 1)
  } else {
    NA_real_
  }
  parameterText <- str_c(
    vapply(
      fittedModel$parameters,
      function(parameterName){
        str_c(
          parameterName,
          "=",
          format(
            fittedModel[[parameterName]],
            digits = 10,
            scientific = TRUE,
            trim = TRUE
          )
        )
      },
      character(1)
    ),
    collapse = "; "
  )
  goodnessResult <- tryCatch(
    sby_internal_distribution_goodness(
      sortedValues = sort(values),
      model = fittedModel,
      distributionName = distributionName,
      tailProbability = tailProbability,
      distributionNamespace =
        distributionRuntime$distributionNamespace
    ),
    error = function(errorCondition){
      list(
        KS_STAT = NA_real_,
        CVM_STAT = NA_real_,
        AD_STAT = NA_real_,
        QTD_OUTLIER_CAUDA = NA_real_,
        PERC_OUTLIER_CAUDA = NA_real_
      )
    }
  )
  negativeConvergenceWarning <- any(
    str_detect(
      str_to_lower(warningMessages),
      str_c(
        "not converg|did not converg|fail(?:ed|ure)? to converg|",
        "convergence problem|non-converg"
      )
    )
  )
  convergenceValue <- fittedModel$convergence
  convergedValue <- fittedModel$converged
  reportedConvergence <- if(!is.null(convergedValue)){
    isTRUE(convergedValue)
  } else if(!is.null(convergenceValue)){
    if(is.logical(convergenceValue)){
      isTRUE(convergenceValue)
    } else if(is.numeric(convergenceValue)){
      all(convergenceValue == 0)
    } else {
      NA
    }
  } else {
    NA
  }

  result$CONVERGIU <- is.finite(globalDeviance) &&
    !negativeConvergenceWarning &&
    !identical(reportedConvergence, FALSE)
  result$FLAG_CONVERGENCIA_REPORTADA <- reportedConvergence
  result$CODIGO_CONVERGENCIA <- if(!is.null(convergenceValue)){
    sby_internal_profile_format_scalar(convergenceValue)
  } else {
    NA_character_
  }
  result$QTD_PARAMETROS <- parameterCount
  result$LOG_VEROSSIMILHANCA <- logLikelihood
  result$AIC <- aicValue
  result$AICC <- aiccValue
  result$BIC <- bicValue
  result$KS_STAT <- goodnessResult$KS_STAT
  result$CVM_STAT <- goodnessResult$CVM_STAT
  result$AD_STAT <- goodnessResult$AD_STAT
  result$QTD_OUTLIER_CAUDA <-
    goodnessResult$QTD_OUTLIER_CAUDA
  result$PERC_OUTLIER_CAUDA <-
    goodnessResult$PERC_OUTLIER_CAUDA
  result$PARAMETROS <- parameterText
  result$AVISO <- if(length(warningMessages) > 0L){
    str_c(unique(warningMessages), collapse = "; ")
  } else {
    NA_character_
  }

  result
}

sby_internal_identify_distribution <- function(
  x,
  candidates,
  maxSampleSize,
  tailProbability,
  modelPrior,
  distributionRuntime
){
  finiteValues <- suppressWarnings(
    as.numeric(x[is.finite(x)])
  )

  if(length(finiteValues) < 20L){
    stop(
      "At least 20 finite observations are required",
      call. = FALSE
    )
  }

  if(uniqueN(finiteValues) < 3L){
    stop(
      "At least three distinct values are required",
      call. = FALSE
    )
  }

  sampledValues <- sby_internal_profile_deterministic_sample(
    values = finiteValues,
    maxSize = maxSampleSize
  )
  candidateData <- as.data.frame(
    candidates,
    stringsAsFactors = FALSE
  )
  fitList <- Map(
    f = function(distributionName, domainName, mixedFlag){
      sby_internal_fit_distribution(
        values = sampledValues,
        distributionName = distributionName,
        domainName = domainName,
        mixedFlag = mixedFlag,
        tailProbability = tailProbability,
        distributionRuntime = distributionRuntime
      )
    },
    distributionName = candidateData$DISTRIBUICAO,
    domainName = candidateData$DOMINIO,
    mixedFlag = candidateData$FLAG_DISTRIBUICAO_MISTA
  )
  resultDt <- rbindlist(
    fitList,
    use.names = TRUE,
    fill = TRUE
  )
  resultDt$QTD_OBSERVACOES_ORIGINAIS <-
    as.numeric(length(finiteValues))
  resultDt$QTD_OBSERVACOES_USADAS <-
    as.numeric(length(sampledValues))
  resultDt$FLAG_AMOSTRADO <-
    length(sampledValues) < length(finiteValues)
  resultDt$PROBABILIDADE_CAUDA_OUTLIER <- tailProbability
  resultDt$DELTA_AIC <- NA_real_
  resultDt$PESO_AKAIKE <- NA_real_
  resultDt$ORDEM_AIC <- NA_real_
  resultDt$FLAG_MELHOR_AIC <- FALSE
  resultDt$DELTA_AICC <- NA_real_
  resultDt$PESO_AKAIKE_AICC <- NA_real_
  resultDt$ORDEM_AICC <- NA_real_
  resultDt$FLAG_MELHOR_AICC <- FALSE
  resultDt$DELTA_BIC <- NA_real_
  resultDt$FATOR_BAYES_RELATIVO_BIC <- NA_real_
  resultDt$PROB_POSTERIOR_BIC_APROX <- NA_real_
  resultDt$ORDEM_BIC <- NA_real_
  resultDt$FLAG_MELHOR_BIC <- FALSE
  resultDt$FLAG_INFERENCIA_BAYESIANA_APROXIMADA <- FALSE
  aicIndexes <- which(
    resultDt$SUPORTE_COMPATIVEL &
      resultDt$CONVERGIU &
      is.finite(resultDt$AIC)
  )

  if(length(aicIndexes) > 0L){
    successfulAic <- resultDt$AIC[aicIndexes]
    deltaAic <- successfulAic - min(successfulAic)
    relativeAicLikelihood <- exp(-0.5 * deltaAic)
    aicWeights <- relativeAicLikelihood /
      sum(relativeAicLikelihood)
    aicOrder <- rank(successfulAic, ties.method = "min")

    resultDt$DELTA_AIC[aicIndexes] <- deltaAic
    resultDt$PESO_AKAIKE[aicIndexes] <- aicWeights
    resultDt$ORDEM_AIC[aicIndexes] <- aicOrder
    resultDt$FLAG_MELHOR_AIC[aicIndexes] <- aicOrder == 1
  }

  aiccIndexes <- which(
    resultDt$SUPORTE_COMPATIVEL &
      resultDt$CONVERGIU &
      is.finite(resultDt$AICC)
  )

  if(length(aiccIndexes) > 0L){
    successfulAicc <- resultDt$AICC[aiccIndexes]
    deltaAicc <- successfulAicc - min(successfulAicc)
    relativeAiccLikelihood <- exp(-0.5 * deltaAicc)
    aiccWeights <- relativeAiccLikelihood /
      sum(relativeAiccLikelihood)
    aiccOrder <- rank(successfulAicc, ties.method = "min")

    resultDt$DELTA_AICC[aiccIndexes] <- deltaAicc
    resultDt$PESO_AKAIKE_AICC[aiccIndexes] <- aiccWeights
    resultDt$ORDEM_AICC[aiccIndexes] <- aiccOrder
    resultDt$FLAG_MELHOR_AICC[aiccIndexes] <-
      aiccOrder == 1
  }

  successIndexes <- which(
    resultDt$SUPORTE_COMPATIVEL &
      resultDt$CONVERGIU &
      is.finite(resultDt$BIC)
  )

  if(length(successIndexes) > 0L){
    successfulNames <- resultDt$DISTRIBUICAO[successIndexes]

    if(is.null(modelPrior)){
      priorValues <- rep(1, length(successIndexes))
    } else {
      if(
        is.null(names(modelPrior)) ||
        any(!is.finite(modelPrior)) ||
        any(modelPrior <= 0)
      ){
        stop(
          "`model_prior` must be a positive named vector",
          call. = FALSE
        )
      }

      priorValues <- as.numeric(modelPrior[successfulNames])

      if(
        any(!is.finite(priorValues)) ||
        any(priorValues <= 0)
      ){
        stop(
          "Missing prior weights for successfully fitted models",
          call. = FALSE
        )
      }
    }

    priorValues <- priorValues / sum(priorValues)
    successfulBic <- resultDt$BIC[successIndexes]
    minimumBic <- min(successfulBic)
    deltaBic <- successfulBic - minimumBic
    relativeBayesFactor <- exp(-0.5 * deltaBic)
    logEvidenceApproximation <- -0.5 * successfulBic +
      log(priorValues)
    evidenceWeights <- exp(
      logEvidenceApproximation -
        max(logEvidenceApproximation)
    )
    posteriorApproximation <- evidenceWeights /
      sum(evidenceWeights)
    bicOrder <- rank(
      successfulBic,
      ties.method = "min"
    )

    resultDt$DELTA_BIC[successIndexes] <- deltaBic
    resultDt$FATOR_BAYES_RELATIVO_BIC[successIndexes] <-
      relativeBayesFactor
    resultDt$PROB_POSTERIOR_BIC_APROX[successIndexes] <-
      posteriorApproximation
    resultDt$ORDEM_BIC[successIndexes] <- bicOrder
    resultDt$FLAG_MELHOR_BIC[successIndexes] <- bicOrder == 1
    resultDt$FLAG_INFERENCIA_BAYESIANA_APROXIMADA[
      successIndexes
    ] <- TRUE
  }

  finiteBic <- ifelse(is.finite(resultDt$BIC), resultDt$BIC, Inf)
  sortOrder <- order(
    !resultDt$SUPORTE_COMPATIVEL,
    !resultDt$CONVERGIU,
    finiteBic,
    resultDt$DISTRIBUICAO
  )

  as_tibble(resultDt[sortOrder])
}
