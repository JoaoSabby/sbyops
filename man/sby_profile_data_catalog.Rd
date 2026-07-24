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
\item{.data}{Data frame ou tibble.}
\item{...}{Expressões tidyselect. Quando omitidas, todas as colunas são perfiladas.}
\item{bitmap_cardinality_ratio}{Razão máxima de cardinalidade distinta usada para sinalizar candidatas a bitmap.}
\item{bitmap_minimum_rows}{Número mínimo de linhas usado pela regra de triagem para bitmap.}
\item{partition_minimum_rows}{Número mínimo de linhas usado pela regra de triagem para particionamento por data.}
\item{max_robust_sample}{Tamanho máximo da amostra determinística usada para estimar o medcouple.}
\item{num_treads}{Inteiro positivo opcional com limite temporário de threads para a chamada.}
}
\value{
Um tibble com uma linha por coluna selecionada e campos de tipo, nulidade, cardinalidade, frequência, estatística descritiva, qualidade textual, outliers, prontidão para modelagem e recomendações físicas baseadas somente nos dados.
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
