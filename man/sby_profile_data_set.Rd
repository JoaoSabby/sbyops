% Generated manually for the development snapshot.
\name{sby_profile_data_set}
\alias{sby_profile_data_set}
\title{Perfil estrutural de um conjunto de dados}
\usage{
sby_profile_data_set(.data, calculate_duplicate_rows = TRUE, num_treads = NULL)
}
\arguments{
\item{.data}{Tabela DuckDB, data frame, tibble ou data.table.}
\item{calculate_duplicate_rows}{Escalar lógico que indica se linhas duplicadas exatas devem ser contadas.}
\item{num_treads}{Inteiro positivo opcional com limite temporário de threads para a chamada.}
}
\value{
Um tibble de uma linha com dimensões, uso aproximado de memória, contagem de células ausentes, linhas completas, linhas totalmente nulas, linhas duplicadas e colisões de nomes.
}
\description{
Resume a estrutura global de um conjunto de dados tabular para auditoria inicial de volume, completude, duplicidade e qualidade de metadados.
}
\details{
A contagem de duplicatas pode ser desativada para reduzir custo em bases muito grandes. O resultado é útil como etapa complementar ao catálogo por coluna, pois quantifica propriedades que dependem da matriz completa e não de variáveis isoladas.
}
\seealso{
\code{\link{sby_profile_data_catalog}}, \code{\link{sby_profile_missing_patterns}}
}
