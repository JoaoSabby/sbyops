% Generated manually for the development snapshot.
\name{sby_select_modal_frequency}
\alias{sby_select_modal_frequency}
\title{Selecionar colunas por frequência modal}
\usage{sby_select_modal_frequency(.data, ..., threshold = 0.99)}
\arguments{
\item{.data}{Data frame, tibble ou matriz.}
\item{...}{Expressões tidyselect. Quando omitidas, todas as colunas são avaliadas.}
\item{threshold}{Escalar numérico no intervalo fechado \eqn{[0,1]}.}
}
\value{Objeto com a mesma classe estrutural de \code{.data}, sem as colunas selecionadas de alta frequência modal.}
\description{Remove colunas cuja categoria ou valor mais frequente representa proporção maior ou igual ao limiar informado.}
\details{A ferramenta identifica variáveis praticamente constantes por meio da frequência do valor modal. Colunas fora da seleção são preservadas.}
\seealso{\code{\link{sby_select_non_constant}}, \code{\link{sby_select_correlation}}}
