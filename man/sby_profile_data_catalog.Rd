% Generated manually for the development snapshot.
\name{sby_profile_data_catalog}
\alias{sby_profile_data_catalog}
\title{Catalogar variáveis tabulares para modelagem e Oracle}
\usage{
sby_profile_data_catalog(
  .data,
  ...,
  bitmap_cardinality_ratio = 0.01,
  bitmap_minimum_rows = 100000L,
  partition_minimum_rows = 1000000L,
  max_robust_sample = 100000L,
  num_treads = NULL
)
}
\arguments{
\item{.data}{Data frame, tibble ou data.table.}
\item{...}{Expressões tidyselect. Quando omitidas, todas as colunas são perfiladas.}
\item{bitmap_cardinality_ratio}{Razão máxima de cardinalidade distinta usada para sinalizar candidatas a bitmap.}
\item{bitmap_minimum_rows}{Número mínimo de linhas usado pela regra de triagem para bitmap.}
\item{partition_minimum_rows}{Número mínimo de linhas usado pela regra de triagem para particionamento por data.}
\item{max_robust_sample}{Tamanho máximo da amostra determinística usada para estimar o medcouple.}
\item{num_treads}{Inteiro positivo opcional com limite temporário de threads para a chamada.}
}

\value{
Tibble com uma linha por coluna selecionada. O catálogo pode incluir os seguintes grupos de cálculos e valores gerados, conforme o tipo da coluna:
\itemize{
\item \strong{Metadados e estrutura da coluna}: \code{NOME_COLUNA}, \code{POSICAO_COLUNA}, \code{CLASSES_R}, \code{TIPO_BASE_R}, \code{MODO_ARMAZENAMENTO_R}, \code{ATRIBUTOS_R}, \code{ROTULO}, \code{UNIDADE}, \code{BYTES_OBJETO_R}, \code{BYTES_R_POR_REGISTRO}, tamanhos do nome da coluna, colisão em maiúsculas, \code{QTD_REGISTROS}, flags de tipo e flags estruturais.
\item \strong{Nulidade e preenchimento}: contagens e percentuais de preenchidos, nulos, \code{NA}, \code{NaN}, infinitos, modelo bayesiano de nulidade e intervalo credível de 95\% para nulidade.
\item \strong{Cardinalidade, frequência e concentração}: distintos, duplicados válidos, moda, frequências top 1/top 2/top 5/top 10, entropia de Shannon, entropia normalizada, cardinalidade efetiva, índice HHI e impureza de Gini.
\item \strong{Estatística numérica, Bayes e outliers}: válidos estatísticos, zeros, positivos, negativos, soma, mínimo, máximo, amplitude, média, erro-padrão, mediana, variância, desvio-padrão, coeficiente de variação, MAD, assimetria, curtose, modelo bayesiano numérico, intervalos da média e predição, quantis \code{P000} a \code{P100}, amplitude interquartil, inteiros empíricos, dígitos inteiros, monotonicidade, limites e contagens de outliers por Tukey, Tukey extremo, z-score, z-score robusto e boxplot ajustado, medcouple, tamanho da amostra robusta e flag contínua.
\item \strong{Texto e fatores}: comprimentos em caracteres e bytes, vazios, espaços, caracteres de controle, quebras de linha, não ASCII, distintos normalizados, colisões de normalização, padrões textuais (inteiro, decimal, lógico, data ISO, datetime ISO, UUID, e-mail, URL e JSON) e metadados de níveis de fator.
\item \strong{Datas e tempos}: data mínima, data máxima, amplitude em dias, anos, meses e dias distintos, fuso horário, monotonicidade temporal e segundos fracionários.
\item \strong{Oracle e armazenamento físico}: sugestão de tipo Oracle, tipo alternativo, alertas, grau de cardinalidade, chave candidata, baixa cardinalidade, candidatos a bitmap, b-tree, histograma, partição, compressão, compatibilidade de nomes Oracle e metadados Oracle a coletar.
\item \strong{Prontidão para modelagem}: quase sem variância, possível identificador, alta nulidade, categoria de alta cardinalidade, revisão antes de modelagem e motivos de revisão.
}
}
\description{
Constrói um catálogo técnico de variáveis para bases tabulares, combinando metadados de tipo, nulidade, cardinalidade, estatística descritiva, qualidade textual, diagnóstico robusto de outliers e sinais físicos para Oracle.
}
\details{
Cada coluna selecionada é analisada isoladamente. Variáveis numéricas recebem medidas de posição, escala, assimetria, curtose, quantis, detecção de inteiros empíricos, intervalos bayesianos sob modelo normal com prior de Jeffreys e regras robustas de outliers, incluindo boxplot ajustado por medcouple. Variáveis textuais recebem medidas de comprimento, normalização Unicode, espaços, caixa, vazios semânticos e potenciais problemas de armazenamento.

Os campos relacionados a bitmap, particionamento, compressão, histograma e índices Oracle são sinais derivados somente dos dados. Eles não substituem workload SQL, seletividade real, estatísticas do otimizador ou validação de engenharia de banco de dados. O processamento usa \pkg{data.table} em cópia privada e respeita o contexto de threads do \pkg{sbyops}.
}
\references{
Tukey, J. W. (1977). \emph{Exploratory Data Analysis}. Addison-Wesley.

Hubert, M.; Vandervieren, E. (2008). An adjusted boxplot for skewed distributions. \emph{Computational Statistics & Data Analysis}, 52, 5186--5201.

Shannon, C. E. (1948). A mathematical theory of communication. \emph{Bell System Technical Journal}, 27, 379--423.
}
\seealso{
\code{\link{sby_profile_data_set}}, \code{\link{sby_profile_missing_patterns}}, \code{\link{sby_profile_data_relations}}
}
