% Generated manually for the development snapshot.
\name{sby_select_non_constant}
\alias{sby_select_non_constant}
\title{Remover colunas constantes}
\usage{sby_select_non_constant(.data, ...)}
\arguments{
\item{.data}{Data frame, tibble ou matriz.}
\item{...}{Expressões tidyselect. Quando omitidas, todas as colunas são avaliadas.}
}
\value{Objeto com a mesma classe estrutural de \code{.data}, sem as colunas constantes selecionadas.}
\description{Remove colunas selecionadas que contêm um único valor repetido ao longo de todas as observações.}
\details{A ferramenta é uma etapa inicial de limpeza para reduzir variáveis sem informação amostral. O backend nativo em C é usado quando disponível; caso contrário, uma implementação R segura é aplicada.}
\examples{constant_data <- data.frame(a = c(1, 1, 1), b = c(1, 2, 3))
sby_select_non_constant(constant_data)}
\seealso{\code{\link{sby_select_correlation}}, \code{\link{sby_select_modal_frequency}}}
