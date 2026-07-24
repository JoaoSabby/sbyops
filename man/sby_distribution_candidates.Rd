% Generated manually for the development snapshot.
\name{sby_distribution_candidates}
\alias{sby_distribution_candidates}
\title{Listar distribuições contínuas candidatas}
\usage{
sby_distribution_candidates()
}
\value{
Um tibble com as colunas \code{DISTRIBUICAO}, \code{DOMINIO} e \code{FLAG_DISTRIBUICAO_MISTA}.
}
\description{
Retorna o catálogo curado de distribuições contínuas candidatas usado por \code{sby_identify_distribution()}, incluindo domínio de suporte e indicação de famílias mistas.
}
\details{
O catálogo organiza famílias GAMLSS por suporte observado: real, positivo e intervalos unitários com ou sem massa nos limites. A tabela é explícita para tornar auditável o conjunto de modelos avaliado em seleção por verossimilhança e critérios de informação.
}
\seealso{
\code{\link{sby_identify_distribution}}
}
