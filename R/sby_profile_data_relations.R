#' @title Profile Pairwise Data Relations
#'
#' @usage
#' sby_profile_data_relations(
#'   .data,
#'   ...,
#'   max_rows = 100000L,
#'   numeric_bins = 10L,
#'   strong_relation_threshold = 0.8,
#'   num_treads = NULL
#' )
#'
#' @description
#' Mede relações bivariadas entre colunas por correlações, associação categórica,
#' razão de correlação, informação mútua normalizada, nulidade conjunta e
#' dependências funcionais empíricas.
#'
#' @details
#' Pares numéricos recebem correlações de Pearson, Spearman e Kendall amostral.
#' Pares categóricos recebem V de Cramér corrigido para viés. Pares mistos usam
#' \(\eta^2\), a proporção da variabilidade numérica explicada por grupos.
#' Todos os pares válidos recebem informação mútua após discretização por
#' quantis quando necessário.
#'
#' A cardinalidade de pares cresce como \(p(p-1)/2\). Por isso, recomenda-se
#' selecionar subconjuntos de colunas em bases largas. Os sinais de redundância,
#' vazamento e extended statistics Oracle são heurísticos e exigem validação com
#' semântica de negócio, tempo de referência e workload SQL.
#'
#' @references
#' Cramér, H. (1946). *Mathematical Methods of Statistics*. Princeton.
#'
#' Cover, T. M.; Thomas, J. A. (2006). *Elements of Information Theory*. Wiley.
#'
#' Kendall, M. G. (1938). A new measure of rank correlation. *Biometrika*,
#' 30(1/2), 81--93.
#'
#' @details
#' Numeric pairs receive Pearson, Spearman, and sampled Kendall correlations.
#' Categorical pairs receive corrected Cramer's V. Mixed pairs receive the
#' correlation ratio. Every supported pair receives normalized mutual
#' information after numeric discretization.
#'
#' Strong relations and functional dependencies can indicate redundant
#' predictors or leakage. Oracle extended-statistics fields are screening
#' signals only. SQL workload and optimizer column usage remain required.
#' The number of pairs grows quadratically, so wide inputs should use `...`
#' to select a relevant subset or branch the work in `targets`.
#'
#' @param .data A data frame or tibble.
#' @param ... Tidyselect expressions. When omitted, all columns are profiled.
#' @param max_rows Positive integer or `Inf`. Larger inputs are sampled
#' deterministically.
#' @param numeric_bins Number of quantile bins used for mutual information.
#' @param strong_relation_threshold Threshold in `(0, 1)` for screening.
#' @param num_treads Optional positive integer thread cap for this call.
#'
#' @return A tibble with one row per column pair.
#'
#' @importFrom stats cor
#' @importFrom utils combn
#' @export
sby_profile_data_relations <- function(
  .data,
  ...,
  max_rows = 100000L,
  numeric_bins = 10L,
  strong_relation_threshold = 0.8,
  num_treads = NULL
){
  # Resolve and sample the selected table before pair enumeration so the
  # quadratic relation scan remains reproducible and bounded in runtime.
  sby_internal_validate_tabular_input(.data = .data)

  maxRows <- if(
    is.numeric(max_rows) &&
    length(max_rows) == 1L &&
    is.infinite(max_rows) &&
    max_rows > 0
  ){
    Inf
  } else {
    sby_internal_profile_validate_positive_integer(
      value = max_rows,
      argumentName = "max_rows"
    )
  }
  numericBins <- sby_internal_profile_validate_positive_integer(
    value = numeric_bins,
    argumentName = "numeric_bins"
  )

  if(numericBins < 2L){
    stop("`numeric_bins` must be at least 2", call. = FALSE)
  }

  strongRelationThreshold <-
    sby_internal_profile_validate_probability(
      value = strong_relation_threshold,
      argumentName = "strong_relation_threshold",
      allowBoundary = FALSE
    )
  requestedThreads <- if(is.null(num_treads)){
    sby_internal_get_max_threads()
  } else {
    sby_internal_validate_max_threads(num_treads)
  }
  selectedColumns <- sby_internal_eval_select(
    .data = .data,
    ...,
    default = "all"
  )

  if(length(selectedColumns) < 2L){
    stop(
      "At least two columns must be selected",
      call. = FALSE
    )
  }

  selectedData <- .data[, unname(selectedColumns), drop = FALSE]

  sby_internal_with_thread_context(
    expr = sby_internal_profile_with_data_table_threads(
      expr = sby_internal_profile_data_relations(
        .data = selectedData,
        maxRows = maxRows,
        numericBins = numericBins,
        strongRelationThreshold = strongRelationThreshold
      ),
      maxThreads = requestedThreads
    ),
    maxThreads = requestedThreads,
    useOpenmp = TRUE,
    useBlas = FALSE
  )
}

sby_internal_profile_relation_is_numeric <- function(values){
  (
    is.integer(values) ||
      is.double(values) ||
      inherits(values, "integer64") ||
      inherits(values, "Date") ||
      inherits(values, "POSIXt")
  ) &&
    !is.factor(values)
}

sby_internal_profile_relation_character <- function(values){
  if(is.atomic(values)){
    return(as.character(values))
  }

  vapply(
    values,
    sby_internal_profile_format_scalar,
    character(1)
  )
}

sby_internal_profile_relation_entropy <- function(values){
  valueCount <- length(values)

  if(valueCount == 0L){
    return(NA_real_)
  }

  frequencies <- tabulate(
    match(values, unique(values))
  )
  probabilities <- frequencies / valueCount

  -sum(probabilities * log2(probabilities))
}

sby_internal_profile_relation_discretize <- function(
  values,
  numericBins
){
  if(!sby_internal_profile_relation_is_numeric(values)){
    return(sby_internal_profile_relation_character(values))
  }

  numericValues <- suppressWarnings(as.numeric(values))
  distinctCount <- uniqueN(numericValues)

  if(distinctCount <= numericBins){
    return(as.character(numericValues))
  }

  binCount <- min(numericBins, distinctCount)
  breakValues <- unique(
    as.numeric(
      fquantile(
        numericValues,
        probs = seq.int(
          from = 0,
          to = 1,
          length.out = binCount + 1L
        ),
        names = FALSE
      )
    )
  )

  if(length(breakValues) < 3L){
    return(as.character(numericValues))
  }

  as.character(
    findInterval(
      numericValues,
      breakValues,
      all.inside = TRUE
    )
  )
}

sby_internal_profile_relation_mutual_information <- function(
  values1,
  values2,
  numericBins
){
  discrete1 <- sby_internal_profile_relation_discretize(
    values = values1,
    numericBins = numericBins
  )
  discrete2 <- sby_internal_profile_relation_discretize(
    values = values2,
    numericBins = numericBins
  )
  entropy1 <- sby_internal_profile_relation_entropy(discrete1)
  entropy2 <- sby_internal_profile_relation_entropy(discrete2)
  pairKeys <- str_c(
    stri_numbytes(discrete1),
    ":",
    discrete1,
    "|",
    stri_numbytes(discrete2),
    ":",
    discrete2
  )
  jointEntropy <- sby_internal_profile_relation_entropy(pairKeys)
  mutualInformation <- max(
    0,
    entropy1 + entropy2 - jointEntropy
  )
  normalizedInformation <- if(
    is.finite(entropy1) &&
    entropy1 > 0 &&
    is.finite(entropy2) &&
    entropy2 > 0
  ){
    min(
      1,
      mutualInformation / sqrt(entropy1 * entropy2)
    )
  } else {
    NA_real_
  }

  list(
    INFORMACAO_MUTUA_BITS = mutualInformation,
    INFORMACAO_MUTUA_NORMALIZADA = normalizedInformation
  )
}

sby_internal_profile_relation_cramers_v <- function(
  values1,
  values2
){
  values1 <- sby_internal_profile_relation_character(values1)
  values2 <- sby_internal_profile_relation_character(values2)
  distinct1 <- uniqueN(values1)
  distinct2 <- uniqueN(values2)
  observationCount <- length(values1)

  if(
    observationCount <= 1L ||
    distinct1 <= 1L ||
    distinct2 <= 1L ||
    as.numeric(distinct1) * as.numeric(distinct2) > 1000000
  ){
    return(NA_real_)
  }

  contingencyTable <- table(values1, values2)
  rowTotals <- rowSums(contingencyTable)
  columnTotals <- colSums(contingencyTable)
  expectedCounts <- outer(
    rowTotals,
    columnTotals,
    "*"
  ) / observationCount
  positiveExpected <- expectedCounts > 0
  chiSquared <- sum(
    (
      contingencyTable[positiveExpected] -
        expectedCounts[positiveExpected]
    ) ^ 2 /
      expectedCounts[positiveExpected]
  )
  rowCount <- nrow(contingencyTable)
  columnCount <- ncol(contingencyTable)
  phiSquared <- chiSquared / observationCount
  correctedPhiSquared <- max(
    0,
    phiSquared -
      (columnCount - 1) * (rowCount - 1) /
      (observationCount - 1)
  )
  correctedRowCount <- rowCount -
    (rowCount - 1) ^ 2 / (observationCount - 1)
  correctedColumnCount <- columnCount -
    (columnCount - 1) ^ 2 / (observationCount - 1)
  denominator <- min(
    correctedRowCount - 1,
    correctedColumnCount - 1
  )

  if(!is.finite(denominator) || denominator <= 0){
    return(NA_real_)
  }

  min(
    1,
    sqrt(correctedPhiSquared / denominator)
  )
}

sby_internal_profile_relation_eta_squared <- function(
  numericValues,
  categoryValues
){
  if(
    length(numericValues) <= 1L ||
    uniqueN(numericValues) <= 1L ||
    uniqueN(categoryValues) <= 1L
  ){
    return(NA_real_)
  }

  overallMean <- fmean(numericValues)
  groupIndexes <- match(
    categoryValues,
    unique(categoryValues)
  )
  groupCounts <- tabulate(groupIndexes)
  groupSums <- rowsum(
    numericValues,
    groupIndexes,
    reorder = FALSE
  )
  groupMeans <- as.numeric(groupSums) / groupCounts
  totalVariation <- sum(
    (numericValues - overallMean) ^ 2
  )

  if(!is.finite(totalVariation) || totalVariation <= 0){
    return(NA_real_)
  }

  min(
    1,
    max(
      0,
      sum(groupCounts * (groupMeans - overallMean) ^ 2) /
        totalVariation
    )
  )
}

sby_internal_profile_relation_phi_missing <- function(
  missing1,
  missing2
){
  probability1 <- mean(missing1)
  probability2 <- mean(missing2)
  denominator <- sqrt(
    probability1 *
      (1 - probability1) *
      probability2 *
      (1 - probability2)
  )

  if(!is.finite(denominator) || denominator == 0){
    return(NA_real_)
  }

  max(
    -1,
    min(
      1,
      (
        mean(missing1 & missing2) -
          probability1 * probability2
      ) / denominator
    )
  )
}

sby_internal_profile_relation_pair <- function(
  values1,
  values2,
  columnName1,
  columnName2,
  originalRowCount,
  sampledRowCount,
  duplicateFlag,
  numericBins,
  strongRelationThreshold
){
  numericFlag1 <- sby_internal_profile_relation_is_numeric(values1)
  numericFlag2 <- sby_internal_profile_relation_is_numeric(values2)
  missing1 <- is.na(values1)
  missing2 <- is.na(values2)
  validFlags <- !missing1 & !missing2
  numericValues1 <- if(numericFlag1){
    suppressWarnings(as.numeric(values1))
  } else {
    numeric(length(values1))
  }
  numericValues2 <- if(numericFlag2){
    suppressWarnings(as.numeric(values2))
  } else {
    numeric(length(values2))
  }

  if(numericFlag1){
    validFlags <- validFlags & is.finite(numericValues1)
  }
  if(numericFlag2){
    validFlags <- validFlags & is.finite(numericValues2)
  }

  validValues1 <- values1[validFlags]
  validValues2 <- values2[validFlags]
  validCount <- length(validValues1)
  validNumeric1 <- numericValues1[validFlags]
  validNumeric2 <- numericValues2[validFlags]
  distinct1 <- if(validCount > 0L){
    tryCatch(
      uniqueN(validValues1),
      error = function(errorCondition) NA_real_
    )
  } else {
    0
  }
  distinct2 <- if(validCount > 0L){
    tryCatch(
      uniqueN(validValues2),
      error = function(errorCondition) NA_real_
    )
  } else {
    0
  }
  relationType <- if(numericFlag1 && numericFlag2){
    "NUMERICO_NUMERICO"
  } else if(!numericFlag1 && !numericFlag2){
    "CATEGORICO_CATEGORICO"
  } else {
    "NUMERICO_CATEGORICO"
  }
  pearsonCorrelation <- NA_real_
  spearmanCorrelation <- NA_real_
  kendallCorrelation <- NA_real_
  kendallCount <- 0
  cramersV <- NA_real_
  etaSquared <- NA_real_

  if(
    numericFlag1 &&
    numericFlag2 &&
    validCount >= 3L &&
    uniqueN(validNumeric1) > 1L &&
    uniqueN(validNumeric2) > 1L
  ){
    pearsonCorrelation <- suppressWarnings(
      cor(validNumeric1, validNumeric2, method = "pearson")
    )
    spearmanCorrelation <- suppressWarnings(
      cor(validNumeric1, validNumeric2, method = "spearman")
    )
    kendallIndexes <- sby_internal_profile_deterministic_sample(
      values = seq_len(validCount),
      maxSize = 5000L
    )
    kendallCount <- length(kendallIndexes)
    kendallCorrelation <- suppressWarnings(
      cor(
        validNumeric1[kendallIndexes],
        validNumeric2[kendallIndexes],
        method = "kendall"
      )
    )
  } else if(
    !numericFlag1 &&
    !numericFlag2 &&
    validCount >= 2L
  ){
    cramersV <- sby_internal_profile_relation_cramers_v(
      values1 = validValues1,
      values2 = validValues2
    )
  } else if(validCount >= 2L){
    etaSquared <- if(numericFlag1){
      sby_internal_profile_relation_eta_squared(
        numericValues = validNumeric1,
        categoryValues =
          sby_internal_profile_relation_character(validValues2)
      )
    } else {
      sby_internal_profile_relation_eta_squared(
        numericValues = validNumeric2,
        categoryValues =
          sby_internal_profile_relation_character(validValues1)
      )
    }
  }

  informationResult <- if(validCount >= 2L){
    sby_internal_profile_relation_mutual_information(
      values1 = validValues1,
      values2 = validValues2,
      numericBins = numericBins
    )
  } else {
    list(
      INFORMACAO_MUTUA_BITS = NA_real_,
      INFORMACAO_MUTUA_NORMALIZADA = NA_real_
    )
  }
  functionalResult <- if(
    validCount > 0L &&
    is.atomic(validValues1) &&
    is.atomic(validValues2)
  ){
    characterValues1 <-
      sby_internal_profile_relation_character(validValues1)
    characterValues2 <-
      sby_internal_profile_relation_character(validValues2)
    pairKeys <- str_c(
      stri_numbytes(characterValues1),
      ":",
      characterValues1,
      "|",
      stri_numbytes(characterValues2),
      ":",
      characterValues2
    )
    pairDistinctCount <- uniqueN(pairKeys)

    list(
      determines12 =
        is.finite(distinct1) &&
        pairDistinctCount == distinct1,
      determines21 =
        is.finite(distinct2) &&
        pairDistinctCount == distinct2
    )
  } else {
    list(
      determines12 = NA,
      determines21 = NA
    )
  }
  relationMetrics <- c(
    abs(pearsonCorrelation),
    abs(spearmanCorrelation),
    abs(kendallCorrelation),
    cramersV,
    etaSquared,
    informationResult$INFORMACAO_MUTUA_NORMALIZADA
  )
  finiteMetrics <- relationMetrics[is.finite(relationMetrics)]
  maximumRelation <- if(length(finiteMetrics) > 0L){
    max(finiteMetrics)
  } else {
    NA_real_
  }
  strongRelation <- is.finite(maximumRelation) &&
    maximumRelation >= strongRelationThreshold
  trivialDependency <- (
    isTRUE(functionalResult$determines12) &&
      is.finite(distinct1) &&
      sby_internal_profile_safe_ratio(
        distinct1,
        validCount
      ) >= 0.95
  ) ||
    (
      isTRUE(functionalResult$determines21) &&
        is.finite(distinct2) &&
        sby_internal_profile_safe_ratio(
          distinct2,
          validCount
        ) >= 0.95
    )
  redundancyFlag <- duplicateFlag ||
    strongRelation ||
    (
      isTRUE(functionalResult$determines12) &&
        isTRUE(functionalResult$determines21)
    )
  oracleSuggestion <- if(
    strongRelation ||
    isTRUE(functionalResult$determines12) ||
    isTRUE(functionalResult$determines21)
  ){
    str_c(
      "AVALIAR EXTENDED STATISTICS COLUMN GROUP COM DBMS_STATS ",
      "SOMENTE SE O WORKLOAD USAR AS COLUNAS EM CONJUNTO"
    )
  } else {
    "SEM SINAL BIVARIADO FORTE"
  }

  list(
    NOME_COLUNA_1 = columnName1,
    NOME_COLUNA_2 = columnName2,
    TIPO_RELACAO = relationType,
    QTD_REGISTROS_ORIGINAIS = as.numeric(originalRowCount),
    QTD_REGISTROS_AMOSTRA = as.numeric(sampledRowCount),
    FLAG_AMOSTRADO =
      sampledRowCount < originalRowCount,
    QTD_PARES_VALIDOS = as.numeric(validCount),
    PERC_PARES_VALIDOS =
      sby_internal_profile_safe_ratio(
        validCount,
        sampledRowCount
      ),
    PERC_NULIDADE_CONJUNTA = if(sampledRowCount > 0L){
      mean(missing1 & missing2)
    } else {
      NA_real_
    },
    PHI_NULIDADE =
      sby_internal_profile_relation_phi_missing(
        missing1 = missing1,
        missing2 = missing2
      ),
    QTD_DISTINTOS_COLUNA_1 = as.numeric(distinct1),
    QTD_DISTINTOS_COLUNA_2 = as.numeric(distinct2),
    CORRELACAO_PEARSON = pearsonCorrelation,
    CORRELACAO_SPEARMAN = spearmanCorrelation,
    CORRELACAO_KENDALL = kendallCorrelation,
    QTD_PARES_KENDALL = as.numeric(kendallCount),
    V_CRAMER_AJUSTADO = cramersV,
    ETA_QUADRADO = etaSquared,
    INFORMACAO_MUTUA_BITS =
      informationResult$INFORMACAO_MUTUA_BITS,
    INFORMACAO_MUTUA_NORMALIZADA =
      informationResult$INFORMACAO_MUTUA_NORMALIZADA,
    MAX_RELACAO_OBSERVADA = maximumRelation,
    FLAG_COLUNAS_IDENTICAS_EXATAS = duplicateFlag,
    FLAG_COLUNA_1_DETERMINA_2_NA_AMOSTRA =
      functionalResult$determines12,
    FLAG_COLUNA_2_DETERMINA_1_NA_AMOSTRA =
      functionalResult$determines21,
    FLAG_DEPENDENCIA_FUNCIONAL_TRIVIAL_ID_NA_AMOSTRA =
      trivialDependency,
    FLAG_RELACAO_FORTE = strongRelation,
    FLAG_POSSIVEL_REDUNDANCIA_MODELO = redundancyFlag,
    FLAG_CANDIDATO_EXTENDED_STATS_ORACLE_POR_DADOS =
      strongRelation ||
      isTRUE(functionalResult$determines12) ||
      isTRUE(functionalResult$determines21),
    SUGESTAO_EXTENDED_STATS_ORACLE = oracleSuggestion
  )
}

sby_internal_profile_data_relations <- function(
  .data,
  maxRows,
  numericBins,
  strongRelationThreshold
){
  dataDt <- as.data.table(.data)
  originalRowCount <- nrow(dataDt)
  sampledIndexes <- sby_internal_profile_deterministic_sample(
    values = seq_len(originalRowCount),
    maxSize = maxRows
  )
  sampledData <- dataDt[sampledIndexes]
  pairIndexes <- combn(
    seq_len(ncol(sampledData)),
    2L
  )
  relationList <- Map(
    f = function(columnIndex1, columnIndex2){
      sby_internal_profile_relation_pair(
        values1 = sampledData[[columnIndex1]],
        values2 = sampledData[[columnIndex2]],
        columnName1 = names(sampledData)[columnIndex1],
        columnName2 = names(sampledData)[columnIndex2],
        originalRowCount = originalRowCount,
        sampledRowCount = nrow(sampledData),
        duplicateFlag = identical(
          dataDt[[columnIndex1]],
          dataDt[[columnIndex2]]
        ),
        numericBins = numericBins,
        strongRelationThreshold = strongRelationThreshold
      )
    },
    columnIndex1 = pairIndexes[1, ],
    columnIndex2 = pairIndexes[2, ]
  )

  as_tibble(
    rbindlist(
      relationList,
      use.names = TRUE,
      fill = TRUE
    )
  )
}
