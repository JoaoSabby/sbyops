% Generated manually for the development snapshot.
\name{sby_profile_data_relations}
\alias{sby_profile_data_relations}
\title{Perfil de relações bivariadas entre colunas}
\usage{
sby_profile_data_relations(
  .data,
  ...,
  max_rows = 100000L,
  numeric_bins = 10L,
  strong_relation_threshold = 0.8,
  num_treads = NULL
)
}
\arguments{
\item{.data}{Tabela DuckDB, data frame, tibble ou data.table.}
\item{...}{Expressões tidyselect. Quando omitidas, todas as colunas são avaliadas.}
\item{max_rows}{Inteiro positivo ou \code{Inf}. Entradas maiores são amostradas deterministicamente.}
\item{numeric_bins}{Número de intervalos por quantis usado na discretização para informação mútua.}
\item{strong_relation_threshold}{Limiar em \eqn{(0,1)} usado para sinalização de relação forte.}
\item{num_treads}{Inteiro positivo opcional com limite temporário de threads para a chamada.}
}
\value{
Um tibble com uma linha por par de colunas, incluindo tipo de relação, pares válidos, associação de nulidade, correlações, V de Cramér, \eqn{\eta^2}, informação mútua, dependências funcionais empíricas e sinais de redundância ou extended statistics Oracle.
}
\description{
Mede relações bivariadas entre colunas por correlações, associação categórica, razão de correlação, informação mútua normalizada, nulidade conjunta e dependências funcionais empíricas.
}
\details{
Pares numéricos recebem correlações de Pearson, Spearman e Kendall amostral. Pares categóricos recebem V de Cramér corrigido para viés. Pares mistos usam \eqn{\eta^2}, a proporção da variabilidade numérica explicada por grupos. Todos os pares válidos recebem informação mútua após discretização por quantis quando necessário.

A cardinalidade de pares cresce como \eqn{p(p-1)/2}. Por isso, recomenda-se selecionar subconjuntos de colunas em bases largas. Os sinais de redundância, vazamento e extended statistics Oracle são heurísticos e exigem validação com semântica de negócio, tempo de referência e workload SQL.
}
\references{
Cramér, H. (1946). \emph{Mathematical Methods of Statistics}. Princeton.

Cover, T. M.; Thomas, J. A. (2006). \emph{Elements of Information Theory}. Wiley.

Kendall, M. G. (1938). A new measure of rank correlation. \emph{Biometrika}, 30(1/2), 81--93.
}
\seealso{
\code{\link{sby_profile_data_catalog}}
}
