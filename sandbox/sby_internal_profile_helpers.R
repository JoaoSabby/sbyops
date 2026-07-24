sby_internal_profile_with_data_table_threads <- function(
  expr,
  maxThreads
){
  previousThreads <- getDTthreads()
  on.exit(
    setDTthreads(threads = previousThreads),
    add = TRUE
  )
  setDTthreads(threads = maxThreads)

  force(expr)
}

sby_internal_profile_validate_probability <- function(
  value,
  argumentName,
  allowBoundary = TRUE
){
  if(
    !is.numeric(value) ||
    length(value) != 1L ||
    is.na(value) ||
    !is.finite(value)
  ){
    rangeText <- if(allowBoundary) "[0, 1]" else "(0, 1)"
    stop(
      str_c("`", argumentName, "` must be a numeric scalar in ", rangeText),
      call. = FALSE
    )
  }

  validRange <- if(allowBoundary){
    value >= 0 && value <= 1
  } else {
    value > 0 && value < 1
  }

  if(!validRange){
    rangeText <- if(allowBoundary) "[0, 1]" else "(0, 1)"
    stop(
      str_c("`", argumentName, "` must be a numeric scalar in ", rangeText),
      call. = FALSE
    )
  }

  as.numeric(value)
}

sby_internal_profile_validate_positive_integer <- function(
  value,
  argumentName
){
  if(
    !is.numeric(value) ||
    length(value) != 1L ||
    is.na(value) ||
    !is.finite(value) ||
    value < 1 ||
    value != floor(value)
  ){
    stop(
      str_c("`", argumentName, "` must be a positive integer scalar"),
      call. = FALSE
    )
  }

  as.integer(value)
}

sby_internal_profile_safe_ratio <- function(numerator, denominator){
  if(
    length(denominator) == 0L ||
    is.na(denominator) ||
    denominator == 0
  ){
    return(NA_real_)
  }

  as.numeric(numerator) / as.numeric(denominator)
}

sby_internal_profile_deterministic_sample <- function(values, maxSize){
  valueCount <- length(values)

  if(
    length(maxSize) == 0L ||
    is.na(maxSize) ||
    is.infinite(maxSize) ||
    valueCount <= maxSize
  ){
    return(values)
  }

  sampleIndexes <- unique(
    as.integer(
      round(
        seq.int(
          from = 1,
          to = valueCount,
          length.out = as.integer(maxSize)
        )
      )
    )
  )

  values[sampleIndexes]
}

sby_internal_profile_format_scalar <- function(value){
  if(length(value) == 0L || all(is.na(value))){
    return(NA_character_)
  }

  if(inherits(value, "POSIXt")){
    timeZone <- attr(value, "tzone")

    if(is.null(timeZone) || length(timeZone) == 0L){
      timeZone <- ""
    }

    return(
      format(
        value[1],
        format = "%Y-%m-%dT%H:%M:%OS6%z",
        tz = timeZone[1]
      )
    )
  }

  if(inherits(value, "Date")){
    return(format(value[1], format = "%Y-%m-%d"))
  }

  formattedValue <- tryCatch(
    as.character(value[[1]]),
    error = function(errorCondition) NA_character_
  )

  if(length(formattedValue) == 0L){
    return(NA_character_)
  }

  str_c(formattedValue, collapse = "; ")
}

sby_internal_profile_attribute_text <- function(values, attributeName){
  attributeValue <- attr(values, attributeName, exact = TRUE)

  if(is.null(attributeValue) || length(attributeValue) == 0L){
    return(NA_character_)
  }

  str_c(as.character(attributeValue), collapse = "; ")
}

sby_internal_profile_frequency <- function(values){
  validValues <- values[!is.na(values)]
  validCount <- length(validValues)
  result <- list(
    QTD_DISTINTOS = 0,
    PERC_DISTINTOS_TOTAL = NA_real_,
    PERC_DISTINTOS_VALIDOS = NA_real_,
    QTD_DUPLICADOS_VALIDOS = 0,
    MODA = NA_character_,
    QTD_MODA = 0,
    PERC_MODA_VALIDOS = NA_real_,
    QTD_SEGUNDA_FREQUENCIA = 0,
    RAZAO_FREQUENCIA_TOP1_TOP2 = NA_real_,
    PERC_TOP5_VALIDOS = NA_real_,
    PERC_TOP10_VALIDOS = NA_real_,
    ENTROPIA_SHANNON_BITS = NA_real_,
    ENTROPIA_NORMALIZADA = NA_real_,
    CARDINALIDADE_EFETIVA = NA_real_,
    INDICE_HHI = NA_real_,
    IMPUREZA_GINI = NA_real_
  )

  if(validCount == 0L){
    return(result)
  }

  frequencyResult <- tryCatch(
    {
      uniqueValues <- unique(validValues)
      valueIndexes <- match(validValues, uniqueValues)
      frequencies <- tabulate(
        valueIndexes,
        nbins = length(uniqueValues)
      )
      sortedFrequencies <- sort(frequencies, decreasing = TRUE)
      probabilityValues <- frequencies / validCount
      distinctCount <- length(uniqueValues)
      modeIndex <- which.max(frequencies)
      entropyValue <- -sum(
        probabilityValues * log2(probabilityValues),
        na.rm = TRUE
      )
      secondFrequency <- if(distinctCount >= 2L){
        sortedFrequencies[2]
      } else {
        0
      }

      list(
        QTD_DISTINTOS = as.numeric(distinctCount),
        PERC_DISTINTOS_TOTAL = NA_real_,
        PERC_DISTINTOS_VALIDOS = sby_internal_profile_safe_ratio(
          distinctCount,
          validCount
        ),
        QTD_DUPLICADOS_VALIDOS = as.numeric(
          validCount - distinctCount
        ),
        MODA = sby_internal_profile_format_scalar(
          uniqueValues[modeIndex]
        ),
        QTD_MODA = as.numeric(sortedFrequencies[1]),
        PERC_MODA_VALIDOS = sby_internal_profile_safe_ratio(
          sortedFrequencies[1],
          validCount
        ),
        QTD_SEGUNDA_FREQUENCIA = as.numeric(secondFrequency),
        RAZAO_FREQUENCIA_TOP1_TOP2 = if(secondFrequency > 0){
          sortedFrequencies[1] / secondFrequency
        } else {
          NA_real_
        },
        PERC_TOP5_VALIDOS = sby_internal_profile_safe_ratio(
          sum(head(sortedFrequencies, 5L)),
          validCount
        ),
        PERC_TOP10_VALIDOS = sby_internal_profile_safe_ratio(
          sum(head(sortedFrequencies, 10L)),
          validCount
        ),
        ENTROPIA_SHANNON_BITS = entropyValue,
        ENTROPIA_NORMALIZADA = if(distinctCount > 1L){
          entropyValue / log2(distinctCount)
        } else {
          0
        },
        CARDINALIDADE_EFETIVA = 2 ^ entropyValue,
        INDICE_HHI = sum(probabilityValues ^ 2),
        IMPUREZA_GINI = 1 - sum(probabilityValues ^ 2)
      )
    },
    error = function(errorCondition) NULL
  )

  if(is.null(frequencyResult)){
    distinctCount <- tryCatch(
      uniqueN(validValues),
      error = function(errorCondition) NA_real_
    )
    result$QTD_DISTINTOS <- as.numeric(distinctCount)
    result$PERC_DISTINTOS_VALIDOS <- sby_internal_profile_safe_ratio(
      distinctCount,
      validCount
    )
    result$QTD_DUPLICADOS_VALIDOS <- if(is.na(distinctCount)){
      NA_real_
    } else {
      as.numeric(validCount - distinctCount)
    }
    return(result)
  }

  frequencyResult
}

sby_internal_profile_numeric <- function(
  values,
  rowCount,
  maxRobustSample
){
  numericValues <- suppressWarnings(as.numeric(values))
  finiteValues <- numericValues[is.finite(numericValues)]
  finiteCount <- length(finiteValues)
  result <- list(
    QTD_VALIDOS_ESTATISTICA = as.numeric(finiteCount),
    QTD_ZERO = NA_real_,
    PERC_ZERO_TOTAL = NA_real_,
    PERC_ZERO_VALIDOS = NA_real_,
    QTD_POSITIVOS = NA_real_,
    PERC_POSITIVOS_VALIDOS = NA_real_,
    QTD_NEGATIVOS = NA_real_,
    PERC_NEGATIVOS_TOTAL = NA_real_,
    PERC_NEGATIVOS_VALIDOS = NA_real_,
    SOMA = NA_real_,
    MINIMO = NA_real_,
    MAXIMO = NA_real_,
    AMPLITUDE = NA_real_,
    MEDIA = NA_real_,
    ERRO_PADRAO_MEDIA = NA_real_,
    MEDIANA = NA_real_,
    VARIANCIA = NA_real_,
    DESVIO_PADRAO = NA_real_,
    COEFICIENTE_VARIACAO = NA_real_,
    MAD = NA_real_,
    ASSIMETRIA_AJUSTADA = NA_real_,
    CURTOSE_EXCESSO_AJUSTADA = NA_real_,
    MODELO_BAYES_NUMERICO =
      "NORMAL IID COM PRIOR DE JEFFREYS",
    MEDIA_BAYES_POSTERIOR = NA_real_,
    MEDIA_BAYES_IC95_MIN = NA_real_,
    MEDIA_BAYES_IC95_MAX = NA_real_,
    PROB_BAYES_MEDIA_MAIOR_ZERO = NA_real_,
    PREDICAO_BAYES_IC95_MIN = NA_real_,
    PREDICAO_BAYES_IC95_MAX = NA_real_,
    FLAG_BAYES_NORMAL_JEFFREYS_APLICAVEL = FALSE,
    P000 = NA_real_,
    P001 = NA_real_,
    P005 = NA_real_,
    P010 = NA_real_,
    P025 = NA_real_,
    P050 = NA_real_,
    P075 = NA_real_,
    P090 = NA_real_,
    P095 = NA_real_,
    P099 = NA_real_,
    P100 = NA_real_,
    AMPLITUDE_INTERQUARTIL = NA_real_,
    FLAG_INTEIRO_EMPIRICO = NA,
    DIGITOS_INTEIROS_MAX = NA_real_,
    FLAG_MONOTONICA_CRESCENTE = NA,
    FLAG_MONOTONICA_DECRESCENTE = NA,
    OUTLIER_TUKEY_MIN = NA_real_,
    OUTLIER_TUKEY_MAX = NA_real_,
    QTD_OUTLIER_TUKEY = NA_real_,
    PERC_OUTLIER_TUKEY_VALIDOS = NA_real_,
    OUTLIER_TUKEY_EXTREMO_MIN = NA_real_,
    OUTLIER_TUKEY_EXTREMO_MAX = NA_real_,
    QTD_OUTLIER_TUKEY_EXTREMO = NA_real_,
    PERC_OUTLIER_TUKEY_EXTREMO_VALIDOS = NA_real_,
    QTD_OUTLIER_ZSCORE_3 = NA_real_,
    PERC_OUTLIER_ZSCORE_3_VALIDOS = NA_real_,
    QTD_OUTLIER_ZSCORE_ROBUSTO_35 = NA_real_,
    PERC_OUTLIER_ZSCORE_ROBUSTO_35_VALIDOS = NA_real_,
    MEDCOUPLE = NA_real_,
    OUTLIER_BOXPLOT_AJUSTADO_MIN = NA_real_,
    OUTLIER_BOXPLOT_AJUSTADO_MAX = NA_real_,
    QTD_OUTLIER_BOXPLOT_AJUSTADO = NA_real_,
    PERC_OUTLIER_BOXPLOT_AJUSTADO_VALIDOS = NA_real_,
    QTD_AMOSTRA_ROBUSTA = 0
  )

  if(finiteCount == 0L){
    return(result)
  }

  zeroCount <- sum(finiteValues == 0)
  positiveCount <- sum(finiteValues > 0)
  negativeCount <- sum(finiteValues < 0)
  meanValue <- fmean(finiteValues)
  standardDeviation <- if(finiteCount >= 2L){
    fsd(finiteValues)
  } else {
    NA_real_
  }
  quantileValues <- fquantile(
    finiteValues,
    probs = c(
      0,
      0.01,
      0.05,
      0.10,
      0.25,
      0.50,
      0.75,
      0.90,
      0.95,
      0.99,
      1
    ),
    na.rm = TRUE,
    names = FALSE
  )
  medianValue <- quantileValues[6]
  interquartileRange <- quantileValues[7] - quantileValues[5]
  rawMad <- fmedian(abs(finiteValues - medianValue))
  scaledMad <- rawMad * 1.482602218505602
  standardizedValues <- if(
    finiteCount >= 2L &&
    is.finite(standardDeviation) &&
    standardDeviation > 0
  ){
    (finiteValues - meanValue) / standardDeviation
  } else {
    rep(NA_real_, finiteCount)
  }
  adjustedSkewness <- if(
    finiteCount >= 3L &&
    all(is.finite(standardizedValues))
  ){
    finiteCount /
      ((finiteCount - 1) * (finiteCount - 2)) *
      sum(standardizedValues ^ 3)
  } else {
    NA_real_
  }
  adjustedKurtosis <- if(
    finiteCount >= 4L &&
    all(is.finite(standardizedValues))
  ){
    finiteCount * (finiteCount + 1) /
      ((finiteCount - 1) * (finiteCount - 2) * (finiteCount - 3)) *
      sum(standardizedValues ^ 4) -
      3 * (finiteCount - 1) ^ 2 /
      ((finiteCount - 2) * (finiteCount - 3))
  } else {
    NA_real_
  }
  integerLike <- if(inherits(values, "integer64")){
    TRUE
  } else if(is.integer(values)){
    integerMetadata <- sby_internal_table_detect_integer_type(values)
    integerMetadata[1] == 1
  } else {
    numericMetadata <- sby_internal_table_detect_numeric_type(values)
    numericMetadata[1] == 1 && numericMetadata[3] == 1
  }
  maxAbsoluteValue <- max(abs(finiteValues))
  maxIntegerDigits <- if(maxAbsoluteValue < 1){
    1
  } else {
    floor(log10(maxAbsoluteValue)) + 1
  }
  orderDifferences <- if(finiteCount >= 2L){
    diff(finiteValues)
  } else {
    numeric()
  }
  tukeyLower <- quantileValues[5] - 1.5 * interquartileRange
  tukeyUpper <- quantileValues[7] + 1.5 * interquartileRange
  extremeLower <- quantileValues[5] - 3 * interquartileRange
  extremeUpper <- quantileValues[7] + 3 * interquartileRange
  tukeyFlags <- finiteValues < tukeyLower |
    finiteValues > tukeyUpper
  extremeFlags <- finiteValues < extremeLower |
    finiteValues > extremeUpper
  zScoreFlags <- if(
    is.finite(standardDeviation) &&
    standardDeviation > 0
  ){
    abs(standardizedValues) > 3
  } else {
    rep(FALSE, finiteCount)
  }
  robustZScoreFlags <- if(is.finite(rawMad) && rawMad > 0){
    0.674489750196082 *
      abs(finiteValues - medianValue) /
      rawMad > 3.5
  } else {
    rep(FALSE, finiteCount)
  }
  robustSample <- sby_internal_profile_deterministic_sample(
    values = finiteValues,
    maxSize = maxRobustSample
  )
  medcoupleValue <- tryCatch(
    suppressWarnings(mc(robustSample)),
    error = function(errorCondition) NA_real_
  )
  adjustedLower <- NA_real_
  adjustedUpper <- NA_real_

  if(is.finite(medcoupleValue)){
    if(medcoupleValue >= 0){
      adjustedLower <- quantileValues[5] -
        1.5 * exp(-4 * medcoupleValue) * interquartileRange
      adjustedUpper <- quantileValues[7] +
        1.5 * exp(3 * medcoupleValue) * interquartileRange
    } else {
      adjustedLower <- quantileValues[5] -
        1.5 * exp(-3 * medcoupleValue) * interquartileRange
      adjustedUpper <- quantileValues[7] +
        1.5 * exp(4 * medcoupleValue) * interquartileRange
    }
  }

  adjustedFlags <- if(
    is.finite(adjustedLower) &&
    is.finite(adjustedUpper)
  ){
    finiteValues < adjustedLower |
      finiteValues > adjustedUpper
  } else {
    rep(FALSE, finiteCount)
  }

  result$QTD_ZERO <- as.numeric(zeroCount)
  result$PERC_ZERO_TOTAL <- sby_internal_profile_safe_ratio(
    zeroCount,
    rowCount
  )
  result$PERC_ZERO_VALIDOS <- sby_internal_profile_safe_ratio(
    zeroCount,
    finiteCount
  )
  result$QTD_POSITIVOS <- as.numeric(positiveCount)
  result$PERC_POSITIVOS_VALIDOS <- sby_internal_profile_safe_ratio(
    positiveCount,
    finiteCount
  )
  result$QTD_NEGATIVOS <- as.numeric(negativeCount)
  result$PERC_NEGATIVOS_TOTAL <- sby_internal_profile_safe_ratio(
    negativeCount,
    rowCount
  )
  result$PERC_NEGATIVOS_VALIDOS <- sby_internal_profile_safe_ratio(
    negativeCount,
    finiteCount
  )
  result$SOMA <- fsum(finiteValues)
  result$MINIMO <- quantileValues[1]
  result$MAXIMO <- quantileValues[11]
  result$AMPLITUDE <- quantileValues[11] - quantileValues[1]
  result$MEDIA <- meanValue
  result$ERRO_PADRAO_MEDIA <- if(is.finite(standardDeviation)){
    standardDeviation / sqrt(finiteCount)
  } else {
    NA_real_
  }
  result$MEDIANA <- medianValue
  result$VARIANCIA <- standardDeviation ^ 2
  result$DESVIO_PADRAO <- standardDeviation
  result$COEFICIENTE_VARIACAO <- if(
    is.finite(meanValue) &&
    meanValue != 0 &&
    is.finite(standardDeviation)
  ){
    standardDeviation / abs(meanValue)
  } else {
    NA_real_
  }
  result$MAD <- scaledMad
  result$ASSIMETRIA_AJUSTADA <- adjustedSkewness
  result$CURTOSE_EXCESSO_AJUSTADA <- adjustedKurtosis
  result$MEDIA_BAYES_POSTERIOR <- meanValue

  if(
    finiteCount >= 2L &&
    is.finite(standardDeviation) &&
    standardDeviation > 0
  ){
    posteriorDegreesFreedom <- finiteCount - 1
    posteriorMeanScale <- standardDeviation / sqrt(finiteCount)
    posteriorCriticalValue <- qt(
      0.975,
      df = posteriorDegreesFreedom
    )
    predictiveScale <- standardDeviation *
      sqrt(1 + 1 / finiteCount)

    result$MEDIA_BAYES_IC95_MIN <- meanValue -
      posteriorCriticalValue * posteriorMeanScale
    result$MEDIA_BAYES_IC95_MAX <- meanValue +
      posteriorCriticalValue * posteriorMeanScale
    result$PROB_BAYES_MEDIA_MAIOR_ZERO <- pt(
      meanValue / posteriorMeanScale,
      df = posteriorDegreesFreedom
    )
    result$PREDICAO_BAYES_IC95_MIN <- meanValue -
      posteriorCriticalValue * predictiveScale
    result$PREDICAO_BAYES_IC95_MAX <- meanValue +
      posteriorCriticalValue * predictiveScale
    result$FLAG_BAYES_NORMAL_JEFFREYS_APLICAVEL <- TRUE
  }

  result$P000 <- quantileValues[1]
  result$P001 <- quantileValues[2]
  result$P005 <- quantileValues[3]
  result$P010 <- quantileValues[4]
  result$P025 <- quantileValues[5]
  result$P050 <- quantileValues[6]
  result$P075 <- quantileValues[7]
  result$P090 <- quantileValues[8]
  result$P095 <- quantileValues[9]
  result$P099 <- quantileValues[10]
  result$P100 <- quantileValues[11]
  result$AMPLITUDE_INTERQUARTIL <- interquartileRange
  result$FLAG_INTEIRO_EMPIRICO <- integerLike
  result$DIGITOS_INTEIROS_MAX <- as.numeric(maxIntegerDigits)
  result$FLAG_MONOTONICA_CRESCENTE <- if(finiteCount >= 2L){
    all(orderDifferences >= 0)
  } else {
    NA
  }
  result$FLAG_MONOTONICA_DECRESCENTE <- if(finiteCount >= 2L){
    all(orderDifferences <= 0)
  } else {
    NA
  }
  result$OUTLIER_TUKEY_MIN <- tukeyLower
  result$OUTLIER_TUKEY_MAX <- tukeyUpper
  result$QTD_OUTLIER_TUKEY <- as.numeric(sum(tukeyFlags))
  result$PERC_OUTLIER_TUKEY_VALIDOS <-
    sby_internal_profile_safe_ratio(
      sum(tukeyFlags),
      finiteCount
    )
  result$OUTLIER_TUKEY_EXTREMO_MIN <- extremeLower
  result$OUTLIER_TUKEY_EXTREMO_MAX <- extremeUpper
  result$QTD_OUTLIER_TUKEY_EXTREMO <- as.numeric(
    sum(extremeFlags)
  )
  result$PERC_OUTLIER_TUKEY_EXTREMO_VALIDOS <-
    sby_internal_profile_safe_ratio(
      sum(extremeFlags),
      finiteCount
    )
  result$QTD_OUTLIER_ZSCORE_3 <- as.numeric(sum(zScoreFlags))
  result$PERC_OUTLIER_ZSCORE_3_VALIDOS <-
    sby_internal_profile_safe_ratio(
      sum(zScoreFlags),
      finiteCount
    )
  result$QTD_OUTLIER_ZSCORE_ROBUSTO_35 <- as.numeric(
    sum(robustZScoreFlags)
  )
  result$PERC_OUTLIER_ZSCORE_ROBUSTO_35_VALIDOS <-
    sby_internal_profile_safe_ratio(
      sum(robustZScoreFlags),
      finiteCount
    )
  result$MEDCOUPLE <- medcoupleValue
  result$OUTLIER_BOXPLOT_AJUSTADO_MIN <- adjustedLower
  result$OUTLIER_BOXPLOT_AJUSTADO_MAX <- adjustedUpper
  result$QTD_OUTLIER_BOXPLOT_AJUSTADO <- if(
    is.finite(adjustedLower) &&
    is.finite(adjustedUpper)
  ){
    as.numeric(sum(adjustedFlags))
  } else {
    NA_real_
  }
  result$PERC_OUTLIER_BOXPLOT_AJUSTADO_VALIDOS <- if(
    is.finite(adjustedLower) &&
    is.finite(adjustedUpper)
  ){
    sby_internal_profile_safe_ratio(
      sum(adjustedFlags),
      finiteCount
    )
  } else {
    NA_real_
  }
  result$QTD_AMOSTRA_ROBUSTA <- as.numeric(length(robustSample))

  result
}

sby_internal_profile_text <- function(values){
  validValues <- as.character(values[!is.na(values)])
  validCount <- length(validValues)
  result <- list(
    MIN_COMPRIMENTO_CARACTER = NA_real_,
    MAX_COMPRIMENTO_CARACTER = NA_real_,
    MEDIA_COMPRIMENTO_CARACTER = NA_real_,
    P050_COMPRIMENTO_CARACTER = NA_real_,
    P095_COMPRIMENTO_CARACTER = NA_real_,
    P099_COMPRIMENTO_CARACTER = NA_real_,
    MIN_COMPRIMENTO_BYTES = NA_real_,
    MAX_COMPRIMENTO_BYTES = NA_real_,
    MEDIA_COMPRIMENTO_BYTES = NA_real_,
    QTD_VAZIOS = 0,
    PERC_VAZIOS_VALIDOS = NA_real_,
    QTD_APENAS_ESPACOS = 0,
    QTD_ESPACOS_NAS_BORDAS = 0,
    QTD_CARACTERES_CONTROLE = 0,
    QTD_QUEBRA_LINHA = 0,
    QTD_VALORES_NAO_ASCII = 0,
    PERC_VALORES_NAO_ASCII = NA_real_,
    QTD_DISTINTOS_NORMALIZADOS = NA_real_,
    QTD_COLISOES_NORMALIZACAO = NA_real_,
    PERC_PADRAO_INTEIRO = NA_real_,
    PERC_PADRAO_DECIMAL = NA_real_,
    PERC_PADRAO_LOGICO = NA_real_,
    PERC_PADRAO_DATA_ISO = NA_real_,
    PERC_PADRAO_DATETIME_ISO = NA_real_,
    PERC_PADRAO_UUID = NA_real_,
    PERC_PADRAO_EMAIL = NA_real_,
    PERC_PADRAO_URL = NA_real_,
    PERC_PADRAO_JSON = NA_real_,
    QTD_NIVEIS_FATOR = NA_real_,
    QTD_NIVEIS_NAO_USADOS = NA_real_,
    MAX_COMPRIMENTO_NIVEL_FATOR = NA_real_,
    MAX_BYTES_NIVEL_FATOR = NA_real_,
    FLAG_FATOR_ORDENADO = NA
  )

  if(is.factor(values)){
    factorLevels <- levels(values)
    observedLevels <- unique(
      as.character(values[!is.na(values)])
    )
    result$QTD_NIVEIS_FATOR <- as.numeric(nlevels(values))
    result$QTD_NIVEIS_NAO_USADOS <- as.numeric(
      nlevels(values) - length(observedLevels)
    )
    result$MAX_COMPRIMENTO_NIVEL_FATOR <- if(
      length(factorLevels) > 0L
    ){
      as.numeric(max(stri_length(factorLevels)))
    } else {
      NA_real_
    }
    result$MAX_BYTES_NIVEL_FATOR <- if(
      length(factorLevels) > 0L
    ){
      as.numeric(max(stri_numbytes(factorLevels)))
    } else {
      NA_real_
    }
    result$FLAG_FATOR_ORDENADO <- is.ordered(values)
  }

  if(validCount == 0L){
    return(result)
  }

  characterLengths <- stri_length(validValues)
  byteLengths <- stri_numbytes(validValues)
  lengthQuantiles <- fquantile(
    as.numeric(characterLengths),
    probs = c(0.50, 0.95, 0.99),
    names = FALSE
  )
  normalizedValues <- str_to_lower(
    str_squish(
      stri_trans_nfkc(validValues)
    )
  )
  rawDistinctCount <- uniqueN(validValues)
  normalizedDistinctCount <- uniqueN(normalizedValues)
  nonAsciiFlags <- str_detect(validValues, "[^\\x00-\\x7F]")

  result$MIN_COMPRIMENTO_CARACTER <- as.numeric(
    min(characterLengths)
  )
  result$MAX_COMPRIMENTO_CARACTER <- as.numeric(
    max(characterLengths)
  )
  result$MEDIA_COMPRIMENTO_CARACTER <- fmean(characterLengths)
  result$P050_COMPRIMENTO_CARACTER <- lengthQuantiles[1]
  result$P095_COMPRIMENTO_CARACTER <- lengthQuantiles[2]
  result$P099_COMPRIMENTO_CARACTER <- lengthQuantiles[3]
  result$MIN_COMPRIMENTO_BYTES <- as.numeric(min(byteLengths))
  result$MAX_COMPRIMENTO_BYTES <- as.numeric(max(byteLengths))
  result$MEDIA_COMPRIMENTO_BYTES <- fmean(byteLengths)
  result$QTD_VAZIOS <- as.numeric(sum(validValues == ""))
  result$PERC_VAZIOS_VALIDOS <- sby_internal_profile_safe_ratio(
    result$QTD_VAZIOS,
    validCount
  )
  result$QTD_APENAS_ESPACOS <- as.numeric(
    sum(str_detect(validValues, "^\\s+$"))
  )
  result$QTD_ESPACOS_NAS_BORDAS <- as.numeric(
    sum(validValues != str_trim(validValues))
  )
  result$QTD_CARACTERES_CONTROLE <- as.numeric(
    sum(str_detect(validValues, "[\\p{Cc}\\p{Cf}]"))
  )
  result$QTD_QUEBRA_LINHA <- as.numeric(
    sum(str_detect(validValues, "[\\r\\n]"))
  )
  result$QTD_VALORES_NAO_ASCII <- as.numeric(
    sum(nonAsciiFlags)
  )
  result$PERC_VALORES_NAO_ASCII <-
    sby_internal_profile_safe_ratio(
      sum(nonAsciiFlags),
      validCount
    )
  result$QTD_DISTINTOS_NORMALIZADOS <- as.numeric(
    normalizedDistinctCount
  )
  result$QTD_COLISOES_NORMALIZACAO <- as.numeric(
    rawDistinctCount - normalizedDistinctCount
  )
  result$PERC_PADRAO_INTEIRO <- mean(
    str_detect(validValues, "^[+-]?[0-9]+$")
  )
  result$PERC_PADRAO_DECIMAL <- mean(
    str_detect(
      validValues,
      "^[+-]?(?:[0-9]+(?:[.,][0-9]+)?|[.,][0-9]+)(?:[eE][+-]?[0-9]+)?$"
    )
  )
  result$PERC_PADRAO_LOGICO <- mean(
    str_to_lower(str_trim(validValues)) %in%
      c(
        "0",
        "1",
        "false",
        "true",
        "f",
        "t",
        "n",
        "s",
        "nao",
        "sim"
      )
  )
  result$PERC_PADRAO_DATA_ISO <- mean(
    str_detect(
      validValues,
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"
    )
  )
  result$PERC_PADRAO_DATETIME_ISO <- mean(
    str_detect(
      validValues,
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}[ T][0-9]{2}:[0-9]{2}:[0-9]{2}"
    )
  )
  result$PERC_PADRAO_UUID <- mean(
    str_detect(
      str_to_lower(validValues),
      str_c(
        "^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-",
        "[89ab][0-9a-f]{3}-[0-9a-f]{12}$"
      )
    )
  )
  result$PERC_PADRAO_EMAIL <- mean(
    str_detect(
      validValues,
      "^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$"
    )
  )
  result$PERC_PADRAO_URL <- mean(
    str_detect(
      str_to_lower(validValues),
      "^(?:https?|ftp)://"
    )
  )
  result$PERC_PADRAO_JSON <- mean(
    str_detect(
      str_trim(validValues),
      "^(?:\\{.*\\}|\\[.*\\])$"
    )
  )

  result
}

sby_internal_profile_date <- function(values){
  numericValues <- suppressWarnings(as.numeric(values))
  finiteFlags <- is.finite(numericValues)
  validValues <- values[finiteFlags]
  finiteValues <- numericValues[finiteFlags]
  validCount <- length(validValues)
  result <- list(
    DATA_MINIMA = NA_character_,
    DATA_MAXIMA = NA_character_,
    AMPLITUDE_DATA_DIAS = NA_real_,
    QTD_ANOS_DISTINTOS = NA_real_,
    QTD_MESES_DISTINTOS = NA_real_,
    QTD_DIAS_DISTINTOS = NA_real_,
    FUSO_HORARIO = sby_internal_profile_attribute_text(
      values,
      "tzone"
    ),
    FLAG_DATA_MONOTONICA_CRESCENTE = NA,
    FLAG_DATA_MONOTONICA_DECRESCENTE = NA,
    QTD_SEGUNDOS_FRACIONARIOS = NA_real_
  )

  if(validCount == 0L){
    return(result)
  }

  minimumIndex <- which.min(finiteValues)
  maximumIndex <- which.max(finiteValues)
  numericDifference <- max(finiteValues) - min(finiteValues)
  dayDivisor <- if(inherits(values, "Date")) 1 else 86400
  orderDifferences <- if(validCount >= 2L){
    diff(finiteValues)
  } else {
    numeric()
  }

  result$DATA_MINIMA <- sby_internal_profile_format_scalar(
    validValues[minimumIndex]
  )
  result$DATA_MAXIMA <- sby_internal_profile_format_scalar(
    validValues[maximumIndex]
  )
  result$AMPLITUDE_DATA_DIAS <- numericDifference / dayDivisor
  result$QTD_ANOS_DISTINTOS <- as.numeric(
    uniqueN(format(validValues, "%Y"))
  )
  result$QTD_MESES_DISTINTOS <- as.numeric(
    uniqueN(format(validValues, "%Y-%m"))
  )
  result$QTD_DIAS_DISTINTOS <- as.numeric(
    uniqueN(format(validValues, "%Y-%m-%d"))
  )
  result$FLAG_DATA_MONOTONICA_CRESCENTE <- if(
    validCount >= 2L
  ){
    all(orderDifferences >= 0)
  } else {
    NA
  }
  result$FLAG_DATA_MONOTONICA_DECRESCENTE <- if(
    validCount >= 2L
  ){
    all(orderDifferences <= 0)
  } else {
    NA
  }
  result$QTD_SEGUNDOS_FRACIONARIOS <- if(
    inherits(values, "POSIXt")
  ){
    as.numeric(
      sum(
        abs(finiteValues - round(finiteValues)) >
          sqrt(.Machine$double.eps)
      )
    )
  } else {
    0
  }

  result
}

sby_internal_profile_oracle <- function(
  profile,
  bitmapCardinalityRatio,
  bitmapMinimumRows,
  partitionMinimumRows
){
  rowCount <- profile$QTD_REGISTROS
  validCount <- profile$QTD_PREENCHIDOS
  distinctCount <- profile$QTD_DISTINTOS
  distinctRatio <- profile$PERC_DISTINTOS_VALIDOS
  modeRatio <- profile$PERC_MODA_VALIDOS
  normalizedEntropy <- profile$ENTROPIA_NORMALIZADA
  flagDate <- profile$FLAG_DATA
  flagDateTime <- profile$FLAG_DATETIME
  flagText <- profile$FLAG_TEXTO || profile$FLAG_FATOR
  flagLogical <- profile$FLAG_LOGICA
  flagInteger64 <- profile$FLAG_INTEGER64
  flagNumeric <- profile$FLAG_NUMERICA
  nameByteCount <- profile$QTD_BYTES_NOME_COLUNA
  oracleType <- "REVISAR TIPO"
  alternativeType <- NA_character_
  typeWarning <- NA_character_

  if(flagDate){
    oracleType <- "DATE"
  } else if(flagDateTime){
    oracleType <- "TIMESTAMP(6)"
    alternativeType <-
      "TIMESTAMP(6) WITH TIME ZONE SE O FUSO FOR SEMANTICO"
  } else if(flagLogical){
    oracleType <- "NUMBER(1,0)"
    typeWarning <- "ORACLE 19C NAO POSSUI BOOLEAN SQL NATIVO"
  } else if(flagInteger64){
    oracleType <- "NUMBER(19,0)"
  } else if(flagNumeric && isTRUE(profile$FLAG_INTEIRO_EMPIRICO)){
    observedDigits <- if(is.finite(profile$DIGITOS_INTEIROS_MAX)){
      max(1, min(38, profile$DIGITOS_INTEIROS_MAX))
    } else {
      38
    }
    oracleType <- str_c(
      "NUMBER(",
      as.integer(observedDigits),
      ",0)"
    )

    if(
      is.finite(profile$DIGITOS_INTEIROS_MAX) &&
      profile$DIGITOS_INTEIROS_MAX > 38
    ){
      typeWarning <- "PRECISAO OBSERVADA EXCEDE NUMBER(38)"
    }
  } else if(flagNumeric){
    oracleType <- "BINARY_DOUBLE"
    alternativeType <-
      "NUMBER(P,S) SE HOUVER ESCALA DECIMAL DE NEGOCIO"
    typeWarning <- str_c(
      "DOUBLE R NAO PRESERVA A ESCALA DECIMAL ORIGINAL. ",
      "P E S NAO DEVEM SER INFERIDOS PELA IMPRESSAO DO VALOR"
    )
  } else if(flagText){
    maxCharacters <- profile$MAX_COMPRIMENTO_CARACTER
    maxBytes <- profile$MAX_COMPRIMENTO_BYTES

    if(
      isTRUE(profile$FLAG_FATOR) &&
      is.finite(profile$MAX_COMPRIMENTO_NIVEL_FATOR)
    ){
      maxCharacters <- max(
        maxCharacters,
        profile$MAX_COMPRIMENTO_NIVEL_FATOR,
        na.rm = TRUE
      )
    }
    if(
      isTRUE(profile$FLAG_FATOR) &&
      is.finite(profile$MAX_BYTES_NIVEL_FATOR)
    ){
      maxBytes <- max(
        maxBytes,
        profile$MAX_BYTES_NIVEL_FATOR,
        na.rm = TRUE
      )
    }

    if(
      is.finite(maxCharacters) &&
      is.finite(maxBytes) &&
      maxCharacters <= 4000 &&
      maxBytes <= 4000
    ){
      declaredCharacters <- max(1, as.integer(maxCharacters))
      oracleType <- str_c(
        "VARCHAR2(",
        declaredCharacters,
        " CHAR)"
      )
      typeWarning <-
        "ADICIONAR MARGEM DE CRESCIMENTO DEFINIDA PELO NEGOCIO"
    } else if(is.finite(maxCharacters) && is.finite(maxBytes)){
      oracleType <- "CLOB"
    } else {
      oracleType <- "VARCHAR2(4000 CHAR)"
      typeWarning <- "COLUNA SEM VALORES PARA DIMENSIONAMENTO"
    }
  } else if(profile$TIPO_BASE_R == "raw"){
    oracleType <- "RAW(1)"
    alternativeType <-
      "BLOB PARA CONTEUDO BINARIO VARIAVEL POR REGISTRO"
  } else if(profile$TIPO_BASE_R == "list"){
    oracleType <- "CLOB"
    alternativeType <-
      "JSON EM VERSOES ORACLE COM TIPO JSON NATIVO"
    typeWarning <-
      "VALIDAR ESTRUTURA E RESTRICAO IS JSON NO ORACLE 19C"
  }

  flagCandidateKey <- (
    rowCount > 0 &&
      profile$QTD_NULOS == 0 &&
      is.finite(distinctCount) &&
      distinctCount == rowCount
  )
  flagLowCardinality <- (
    is.finite(distinctRatio) &&
      distinctCount >= 2 &&
      distinctRatio <= bitmapCardinalityRatio
  )
  flagBitmapCandidate <- (
    rowCount >= bitmapMinimumRows &&
      flagLowCardinality &&
      distinctCount < rowCount
  )
  flagSelectiveBtree <- (
    flagCandidateKey ||
      (
        rowCount >= bitmapMinimumRows &&
          is.finite(distinctRatio) &&
          distinctRatio >= 0.10
      )
  )
  flagSkew <- (
    is.finite(distinctCount) &&
      distinctCount > 1 &&
      (
        (
          is.finite(modeRatio) &&
            modeRatio >= 0.10
        ) ||
          (
            is.finite(normalizedEntropy) &&
              normalizedEntropy < 0.80
          )
      )
  )
  histogramSuggestion <- if(!flagSkew){
    "SEM SINAL UNIVARIADO FORTE"
  } else if(distinctCount <= 254){
    "AVALIAR FREQUENCY HISTOGRAM COM METHOD_OPT SIZE AUTO"
  } else {
    "AVALIAR TOP-FREQUENCY OU HYBRID COM METHOD_OPT SIZE AUTO"
  }
  dataSpan <- profile$AMPLITUDE_DATA_DIAS
  flagPartitionCandidate <- (
    (flagDate || flagDateTime) &&
      rowCount >= partitionMinimumRows &&
      is.finite(dataSpan) &&
      dataSpan >= 365
  )
  duplicateRatio <- sby_internal_profile_safe_ratio(
    profile$QTD_DUPLICADOS_VALIDOS,
    validCount
  )
  flagCompressionCandidate <- (
    is.finite(duplicateRatio) &&
      duplicateRatio >= 0.50
  )
  indexSuggestion <- if(flagCandidateKey){
    "AVALIAR BTREE UNIQUE OU PRIMARY KEY"
  } else if(flagBitmapCandidate){
    "AVALIAR BITMAP SOMENTE EM DW READ-MOSTLY E BAIXO DML"
  } else if(flagSelectiveBtree){
    "AVALIAR BTREE SE PARTICIPAR DE PREDICADOS SELETIVOS"
  } else {
    "NAO INFERIVEL SEM WORKLOAD SQL"
  }
  cardinalityClass <- if(!is.finite(distinctRatio)){
    "INDEFINIDA"
  } else if(distinctRatio <= bitmapCardinalityRatio){
    "BAIXA"
  } else if(distinctRatio <= 0.10){
    "MEDIA"
  } else {
    "ALTA"
  }
  oracleNameSyntaxValid <- (
    nameByteCount <= 128 &&
      str_detect(
        profile$NOME_COLUNA,
        "^[[:alpha:]][[:alnum:]_$#]*$"
      )
  )

  list(
    SUGESTAO_TIPO_ORACLE = oracleType,
    TIPO_ORACLE_ALTERNATIVO = alternativeType,
    ALERTA_TIPO_ORACLE = typeWarning,
    GRAU_CARDINALIDADE_ORACLE = cardinalityClass,
    FLAG_CHAVE_CANDIDATA = flagCandidateKey,
    FLAG_BAIXA_CARDINALIDADE = flagLowCardinality,
    FLAG_CANDIDATO_BITMAP_POR_DADOS = flagBitmapCandidate,
    FLAG_CANDIDATO_BTREE_SELETIVO_POR_DADOS = flagSelectiveBtree,
    SUGESTAO_INDICE_ORACLE = indexSuggestion,
    FLAG_DISTRIBUICAO_ASSIMETRICA = flagSkew,
    FLAG_CANDIDATO_HISTOGRAMA_POR_DADOS = flagSkew,
    SUGESTAO_HISTOGRAMA_ORACLE = histogramSuggestion,
    FLAG_CANDIDATO_PARTICAO_RANGE_INTERVAL =
      flagPartitionCandidate,
    FLAG_CANDIDATO_COMPRESSAO_POR_REPETICAO =
      flagCompressionCandidate,
    FLAG_NOME_ORACLE_NAO_DELIMITADO_SINTATICO =
      oracleNameSyntaxValid,
    FLAG_NOME_ORACLE_REQUER_CHECAGEM_V_RESERVED_WORDS =
      oracleNameSyntaxValid,
    FLAG_NOME_ORACLE_MUDA_CAIXA_SE_NAO_DELIMITADO =
      profile$NOME_COLUNA != str_to_upper(profile$NOME_COLUNA),
    FLAG_NOME_EXCEDE_30_BYTES_PRE_12_2 = nameByteCount > 30,
    FLAG_NOME_EXCEDE_128_BYTES = nameByteCount > 128,
    METADADOS_ORACLE_A_COLETAR = str_c(
      "NUM_DISTINCT, DENSITY, NUM_NULLS, NUM_BUCKETS, HISTOGRAM, ",
      "AVG_COL_LEN, SAMPLE_SIZE, LAST_ANALYZED, STALE_STATS, ",
      "BLEVEL, LEAF_BLOCKS, CLUSTERING_FACTOR, V$RESERVED_WORDS ",
      "E USO EM PREDICADOS"
    )
  )
}

sby_internal_profile_model_readiness <- function(profile){
  frequencyRatio <- profile$RAZAO_FREQUENCIA_TOP1_TOP2
  distinctPercent <- 100 * profile$PERC_DISTINTOS_VALIDOS
  flagNearZeroVariance <- (
    isTRUE(profile$FLAG_CONSTANTE) ||
      (
        is.finite(frequencyRatio) &&
          frequencyRatio > 19 &&
          is.finite(distinctPercent) &&
          distinctPercent <= 10
      )
  )
  flagIdentifier <- (
    isTRUE(profile$FLAG_CHAVE_CANDIDATA) ||
      (
        profile$QTD_PREENCHIDOS >= 100 &&
          is.finite(profile$PERC_DISTINTOS_VALIDOS) &&
          profile$PERC_DISTINTOS_VALIDOS >= 0.98
      )
  )
  flagHighMissingness <- (
    is.finite(profile$PERC_NULOS) &&
      profile$PERC_NULOS >= 0.40
  )
  flagHighCardinalityCategory <- (
    (profile$FLAG_TEXTO || profile$FLAG_FATOR) &&
      profile$QTD_DISTINTOS >= 100 &&
      is.finite(profile$PERC_DISTINTOS_VALIDOS) &&
      profile$PERC_DISTINTOS_VALIDOS >= 0.20
  )
  reviewReasons <- character()

  if(isTRUE(profile$FLAG_TODOS_NULOS)){
    reviewReasons <- c(reviewReasons, "TODOS NULOS")
  }
  if(isTRUE(profile$FLAG_CONSTANTE)){
    reviewReasons <- c(reviewReasons, "CONSTANTE")
  }
  if(flagNearZeroVariance){
    reviewReasons <- c(reviewReasons, "QUASE SEM VARIANCIA")
  }
  if(flagIdentifier){
    reviewReasons <- c(reviewReasons, "POSSIVEL IDENTIFICADOR")
  }
  if(flagHighMissingness){
    reviewReasons <- c(reviewReasons, "ALTA NULIDADE")
  }
  if(flagHighCardinalityCategory){
    reviewReasons <- c(
      reviewReasons,
      "CATEGORIA DE ALTA CARDINALIDADE"
    )
  }
  if(
    isTRUE(profile$FLAG_MONOTONICA_CRESCENTE) ||
      isTRUE(profile$FLAG_MONOTONICA_DECRESCENTE) ||
      isTRUE(profile$FLAG_DATA_MONOTONICA_CRESCENTE) ||
      isTRUE(profile$FLAG_DATA_MONOTONICA_DECRESCENTE)
  ){
    reviewReasons <- c(
      reviewReasons,
      "ORDEM TEMPORAL OU SEQUENCIAL"
    )
  }

  list(
    FLAG_QUASE_SEM_VARIANCIA = flagNearZeroVariance,
    FLAG_POSSIVEL_IDENTIFICADOR_MODELO = flagIdentifier,
    FLAG_ALTA_NULIDADE_MODELO = flagHighMissingness,
    FLAG_CATEGORIA_ALTA_CARDINALIDADE_MODELO =
      flagHighCardinalityCategory,
    FLAG_REVISAR_ANTES_MODELAGEM = length(reviewReasons) > 0L,
    MOTIVOS_REVISAO_MODELAGEM = if(length(reviewReasons) > 0L){
      str_c(unique(reviewReasons), collapse = "; ")
    } else {
      NA_character_
    }
  )
}

sby_internal_profile_column <- function(
  values,
  columnName,
  columnPosition,
  rowCount,
  uppercaseCollision,
  bitmapCardinalityRatio,
  bitmapMinimumRows,
  partitionMinimumRows,
  maxRobustSample
){
  flagDate <- inherits(values, "Date")
  flagDateTime <- inherits(values, "POSIXt")
  flagInteger64 <- inherits(values, "integer64")
  flagNumeric <- (
    is.integer(values) ||
      is.double(values) ||
      flagInteger64
  ) &&
    !flagDate &&
    !flagDateTime &&
    !is.factor(values)
  flagText <- is.character(values)
  flagFactor <- is.factor(values)
  flagLogical <- is.logical(values)
  nanFlags <- if(
    is.double(values) &&
    !flagInteger64 &&
    !flagDate &&
    !flagDateTime
  ){
    is.nan(values)
  } else {
    rep(FALSE, length(values))
  }
  missingFlags <- is.na(values)
  pureNaFlags <- missingFlags & !nanFlags
  numericValues <- if(flagNumeric){
    suppressWarnings(as.numeric(values))
  } else {
    numeric(length(values))
  }
  positiveInfiniteFlags <- if(flagNumeric){
    is.infinite(numericValues) & numericValues > 0
  } else {
    rep(FALSE, length(values))
  }
  negativeInfiniteFlags <- if(flagNumeric){
    is.infinite(numericValues) & numericValues < 0
  } else {
    rep(FALSE, length(values))
  }
  filledCount <- sum(!missingFlags)
  nullCount <- sum(missingFlags)
  missingPosterior <- if(rowCount > 0L){
    posteriorAlpha <- nullCount + 0.5
    posteriorBeta <- rowCount - nullCount + 0.5

    list(
      mean = posteriorAlpha / (posteriorAlpha + posteriorBeta),
      lower = qbeta(0.025, posteriorAlpha, posteriorBeta),
      upper = qbeta(0.975, posteriorAlpha, posteriorBeta)
    )
  } else {
    list(
      mean = NA_real_,
      lower = NA_real_,
      upper = NA_real_
    )
  }
  frequencyProfile <- sby_internal_profile_frequency(values)
  frequencyProfile$PERC_DISTINTOS_TOTAL <-
    sby_internal_profile_safe_ratio(
      frequencyProfile$QTD_DISTINTOS,
      rowCount
    )
  distinctCount <- frequencyProfile$QTD_DISTINTOS
  allNull <- filledCount == 0L
  constantFlag <- (
    filledCount > 0L &&
      is.finite(distinctCount) &&
      distinctCount == 1
  )
  binaryFlag <- if(flagLogical){
    filledCount > 0L
  } else {
    is.finite(distinctCount) && distinctCount == 2
  }
  classText <- if(length(class(values)) > 0L){
    str_c(class(values), collapse = "; ")
  } else {
    NA_character_
  }
  attributeNames <- names(attributes(values))
  result <- list(
    NOME_COLUNA = columnName,
    POSICAO_COLUNA = as.numeric(columnPosition),
    CLASSES_R = classText,
    TIPO_BASE_R = typeof(values),
    MODO_ARMAZENAMENTO_R = storage.mode(values),
    ATRIBUTOS_R = if(length(attributeNames) > 0L){
      str_c(attributeNames, collapse = "; ")
    } else {
      NA_character_
    },
    ROTULO = sby_internal_profile_attribute_text(
      values,
      "label"
    ),
    UNIDADE = sby_internal_profile_attribute_text(
      values,
      "units"
    ),
    BYTES_OBJETO_R = as.numeric(object.size(values)),
    BYTES_R_POR_REGISTRO = sby_internal_profile_safe_ratio(
      as.numeric(object.size(values)),
      rowCount
    ),
    QTD_CARACTERES_NOME_COLUNA = as.numeric(
      stri_length(columnName)
    ),
    QTD_BYTES_NOME_COLUNA = as.numeric(
      stri_numbytes(columnName)
    ),
    FLAG_COLISAO_NOME_MAIUSCULO = uppercaseCollision,
    QTD_REGISTROS = as.numeric(rowCount),
    QTD_PREENCHIDOS = as.numeric(filledCount),
    QTD_NULOS = as.numeric(nullCount),
    QTD_NA = as.numeric(sum(pureNaFlags)),
    QTD_NAN = as.numeric(sum(nanFlags)),
    QTD_INFINITO_POSITIVO = as.numeric(
      sum(positiveInfiniteFlags)
    ),
    QTD_INFINITO_NEGATIVO = as.numeric(
      sum(negativeInfiniteFlags)
    ),
    PERC_PREENCHIDOS = sby_internal_profile_safe_ratio(
      filledCount,
      rowCount
    ),
    PERC_NULOS = sby_internal_profile_safe_ratio(
      nullCount,
      rowCount
    ),
    MODELO_BAYES_NULIDADE =
      "BINOMIAL COM PRIOR JEFFREYS BETA(0.5,0.5)",
    PERC_NULOS_BAYES_POSTERIOR = missingPosterior$mean,
    PERC_NULOS_BAYES_IC95_MIN = missingPosterior$lower,
    PERC_NULOS_BAYES_IC95_MAX = missingPosterior$upper,
    PERC_NA = sby_internal_profile_safe_ratio(
      sum(pureNaFlags),
      rowCount
    ),
    PERC_NAN = sby_internal_profile_safe_ratio(
      sum(nanFlags),
      rowCount
    ),
    PERC_INFINITO = sby_internal_profile_safe_ratio(
      sum(positiveInfiniteFlags) +
        sum(negativeInfiniteFlags),
      rowCount
    ),
    FLAG_NUMERICA = flagNumeric,
    FLAG_TEXTO = flagText,
    FLAG_FATOR = flagFactor,
    FLAG_LOGICA = flagLogical,
    FLAG_DATA = flagDate,
    FLAG_DATETIME = flagDateTime,
    FLAG_INTEGER64 = flagInteger64,
    FLAG_ESTATISTICA_INTEGER64_APROXIMADA = flagInteger64,
    FLAG_LISTA = is.list(values) && !is.data.frame(values),
    FLAG_TODOS_NULOS = allNull,
    FLAG_CONSTANTE = constantFlag,
    FLAG_BINARIA = binaryFlag
  )
  result <- c(result, frequencyProfile)

  if(flagNumeric){
    numericProfile <- sby_internal_profile_numeric(
      values = values,
      rowCount = rowCount,
      maxRobustSample = maxRobustSample
    )
    numericProfile$FLAG_CONTINUA <- (
      numericProfile$QTD_VALIDOS_ESTATISTICA > 0 &&
        !isTRUE(numericProfile$FLAG_INTEIRO_EMPIRICO)
    )
    result <- c(result, numericProfile)
  }

  if(flagText || flagFactor){
    result <- c(
      result,
      sby_internal_profile_text(values)
    )
  }

  if(flagDate || flagDateTime){
    result <- c(
      result,
      sby_internal_profile_date(values)
    )
  }

  result <- c(
    result,
    sby_internal_profile_oracle(
      profile = result,
      bitmapCardinalityRatio = bitmapCardinalityRatio,
      bitmapMinimumRows = bitmapMinimumRows,
      partitionMinimumRows = partitionMinimumRows
    )
  )
  result <- c(
    result,
    sby_internal_profile_model_readiness(result)
  )

  result
}

sby_internal_profile_data_catalog <- function(
  .data,
  bitmapCardinalityRatio,
  bitmapMinimumRows,
  partitionMinimumRows,
  maxRobustSample
){
  dataDt <- as.data.table(.data)
  rowCount <- nrow(dataDt)
  columnNames <- names(dataDt)

  if(ncol(dataDt) == 0L){
    return(tibble())
  }

  uppercaseNames <- str_to_upper(columnNames)
  uppercaseCollision <- duplicated(uppercaseNames) |
    duplicated(uppercaseNames, fromLast = TRUE)
  profileList <- Map(
    f = function(
      values,
      columnName,
      columnPosition,
      collisionFlag
    ){
      sby_internal_profile_column(
        values = values,
        columnName = columnName,
        columnPosition = columnPosition,
        rowCount = rowCount,
        uppercaseCollision = collisionFlag,
        bitmapCardinalityRatio = bitmapCardinalityRatio,
        bitmapMinimumRows = bitmapMinimumRows,
        partitionMinimumRows = partitionMinimumRows,
        maxRobustSample = maxRobustSample
      )
    },
    values = dataDt,
    columnName = columnNames,
    columnPosition = seq_along(columnNames),
    collisionFlag = uppercaseCollision
  )
  profileDt <- rbindlist(
    profileList,
    use.names = TRUE,
    fill = TRUE
  )

  as_tibble(profileDt)
}

sby_internal_profile_data_set <- function(
  .data,
  calculateDuplicateRows
){
  dataDt <- as.data.table(.data)
  rowCount <- nrow(dataDt)
  columnCount <- ncol(dataDt)
  missingByRow <- numeric(rowCount)
  missingByColumn <- numeric(columnCount)

  if(columnCount > 0L && rowCount > 0L){
    for(columnIndex in seq_len(columnCount)){
      columnMissing <- is.na(dataDt[[columnIndex]])
      missingByRow <- missingByRow + columnMissing
      missingByColumn[columnIndex] <- sum(columnMissing)
    }
  }

  duplicateRowCount <- if(
    calculateDuplicateRows &&
    rowCount > 0L
  ){
    tryCatch(
      as.numeric(sum(duplicated(dataDt))),
      error = function(errorCondition) NA_real_
    )
  } else {
    NA_real_
  }
  totalCells <- as.numeric(rowCount) * as.numeric(columnCount)
  totalMissing <- sum(missingByColumn)

  tibble(
    QTD_REGISTROS = as.numeric(rowCount),
    QTD_COLUNAS = as.numeric(columnCount),
    QTD_CELULAS = totalCells,
    BYTES_OBJETO_R = as.numeric(object.size(.data)),
    BYTES_R_POR_REGISTRO = sby_internal_profile_safe_ratio(
      as.numeric(object.size(.data)),
      rowCount
    ),
    QTD_CELULAS_NULAS = as.numeric(totalMissing),
    PERC_CELULAS_NULAS = sby_internal_profile_safe_ratio(
      totalMissing,
      totalCells
    ),
    QTD_LINHAS_COMPLETAS = as.numeric(
      sum(missingByRow == 0)
    ),
    PERC_LINHAS_COMPLETAS = sby_internal_profile_safe_ratio(
      sum(missingByRow == 0),
      rowCount
    ),
    QTD_LINHAS_TODAS_NULAS = as.numeric(
      sum(
        missingByRow == columnCount &
          columnCount > 0L
      )
    ),
    MEDIA_NULOS_POR_LINHA = if(rowCount > 0L){
      fmean(missingByRow)
    } else {
      NA_real_
    },
    MEDIANA_NULOS_POR_LINHA = if(rowCount > 0L){
      fmedian(missingByRow)
    } else {
      NA_real_
    },
    MAX_NULOS_POR_LINHA = if(rowCount > 0L){
      max(missingByRow)
    } else {
      NA_real_
    },
    QTD_LINHAS_DUPLICADAS = duplicateRowCount,
    PERC_LINHAS_DUPLICADAS =
      sby_internal_profile_safe_ratio(
        duplicateRowCount,
        rowCount
      ),
    QTD_NOMES_COLUNA_DUPLICADOS = as.numeric(
      sum(duplicated(names(dataDt)))
    ),
    QTD_COLISOES_NOME_MAIUSCULO = as.numeric(
      sum(
        duplicated(
          str_to_upper(names(dataDt))
        )
      )
    )
  )
}

sby_internal_profile_missing_patterns <- function(
  .data,
  topCount
){
  dataDt <- as.data.table(.data)
  rowCount <- nrow(dataDt)

  if(rowCount == 0L){
    return(
      tibble(
        PADRAO_NULIDADE = character(),
        COLUNAS_NULAS = character(),
        QTD_REGISTROS = numeric(),
        PERC_REGISTROS = numeric()
      )
    )
  }

  if(ncol(dataDt) == 0L){
    return(
      tibble(
        PADRAO_NULIDADE = "",
        COLUNAS_NULAS = NA_character_,
        QTD_REGISTROS = as.numeric(rowCount),
        PERC_REGISTROS = 1
      )
    )
  }

  missingPattern <- rep("", rowCount)

  for(columnIndex in seq_len(ncol(dataDt))){
    missingPattern <- str_c(
      missingPattern,
      ifelse(
        is.na(dataDt[[columnIndex]]),
        "1",
        "0"
      )
    )
  }

  patternCounts <- sort(
    table(missingPattern),
    decreasing = TRUE
  )
  retainedIndexes <- seq_len(
    min(length(patternCounts), topCount)
  )
  retainedPatterns <- names(patternCounts)[retainedIndexes]
  retainedCounts <- as.numeric(patternCounts[retainedIndexes])
  columnNames <- names(dataDt)
  missingColumns <- vapply(
    retainedPatterns,
    function(patternValue){
      patternFlags <- strsplit(
        patternValue,
        split = "",
        fixed = TRUE
      )[[1]] == "1"
      missingNames <- columnNames[patternFlags]

      if(length(missingNames) == 0L){
        return(NA_character_)
      }

      str_c(missingNames, collapse = "; ")
    },
    character(1)
  )

  tibble(
    PADRAO_NULIDADE = retainedPatterns,
    COLUNAS_NULAS = missingColumns,
    QTD_REGISTROS = retainedCounts,
    PERC_REGISTROS = retainedCounts / rowCount
  )
}
