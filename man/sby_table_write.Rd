% Generated manually for the development snapshot.
\name{sby_table_write}
\alias{sby_table_write}
\title{Escrever tabela em Parquet com schema Arrow otimizado}
\usage{sby_table_write(.data, file)}
\arguments{
\item{.data}{Data frame, tibble ou data.table a ser escrito.}
\item{file}{Caminho completo de saída. A extensão \code{.parquet} é adicionada quando ausente.}
}
\value{Retorna \code{NULL} invisivelmente.}
\description{Recebe uma tabela R, aplica otimização automática de schema Arrow e grava o resultado em arquivo Parquet.}
\details{A função define internamente schema, compressão, dictionary encoding, estatísticas, tamanho de página, row group e versão Parquet com foco em leitura analítica eficiente. O codec de compressão pode ser ajustado por \code{options(sby_parquet_compression = ...)}.}
\examples{\dontrun{sby_table_write(.data = dados, file = "dados.parquet")}}
\seealso{\code{\link{sby_table_optimize_scheme}}}
