% Generated manually for the development snapshot.
\name{sby_select_correlation}
\alias{sby_select_correlation}
\title{Selecionar colunas por correlação de Pearson}
\usage{sby_select_correlation(.data, ..., threshold, num_treads = NULL)}
\arguments{
\item{.data}{Data frame, tibble, data.table ou matriz numericamente compatível.}
\item{...}{Expressões tidyselect. Quando omitidas, colunas numéricas são avaliadas.}
\item{threshold}{Escalar numérico em \eqn{[0,1]} usado como limiar de remoção.}
\item{num_treads}{Inteiro positivo opcional com limite temporário de threads.}
}
\value{Objeto tabular com colunas correlacionadas removidas.}
\description{Remove colunas numéricas redundantes quando a correlação absoluta de Pearson atinge ou excede um limiar definido pelo usuário.}
\details{A função calcula dependência linear par a par e remove, em cada conflito, a variável com maior correlação média absoluta contra as demais candidatas ativas. A estratégia de execução é escolhida automaticamente entre rotinas de streaming, Fortran e BLAS.}
\seealso{\code{\link{sby_select_non_constant}}, \code{\link{sby_select_modal_frequency}}, \code{\link{sby_config}}}
