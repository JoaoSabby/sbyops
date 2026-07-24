% Generated manually for the development snapshot.
\name{sby_table_optimize_scheme}
\alias{sby_table_optimize_scheme}
\title{Otimizar schema Arrow para escrita Parquet}
\usage{sby_table_optimize_scheme(.data)}
\arguments{\item{.data}{Tabela DuckDB, data frame, tibble ou data.table cujas colunas serão analisadas.}}
\value{Objeto \code{arrow::Schema} otimizado para escrita Parquet.}
\description{Inspeciona colunas de uma tabela R e retorna um schema Arrow compacto para escrita analítica em Parquet.}
\details{A função mapeia tipos lógicos, caracteres, fatores, datas, inteiros e números reais para representações Arrow seguras e compactas. Colunas numéricas são avaliadas por rotinas C++ para reduzir passagens sobre os dados e evitar coerções incorretas de infinitos ou inteiros acima do limite de representação exata.}
\examples{\dontrun{schema_arrow <- sby_table_optimize_scheme(.data = dados)}}
\seealso{\code{\link{sby_table_write}}}
