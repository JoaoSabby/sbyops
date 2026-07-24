% Generated manually for the development snapshot.
\name{sby_profile_missing_patterns}
\alias{sby_profile_missing_patterns}
\title{Perfil de padrões de nulidade}
\usage{
sby_profile_missing_patterns(.data, top_count = 20L, num_treads = NULL)
}
\arguments{
\item{.data}{Tabela DuckDB, data frame, tibble ou data.table.}
\item{top_count}{Inteiro positivo com a quantidade máxima de padrões retornados.}
\item{num_treads}{Inteiro positivo opcional com limite temporário de threads para a chamada.}
}
\value{
Um tibble ordenado por frequência decrescente, contendo representação binária do padrão, nomes das colunas nulas, contagem e proporção de registros.
}
\description{
Retorna os padrões de nulidade mais frequentes em nível de linha para diagnóstico de mecanismos de ausência e problemas de integração de dados.
}
\details{
Cada coluna contribui com um indicador binário no padrão de nulidade. O resultado permite identificar ausências estruturais, blocos de campos que faltam conjuntamente e potenciais regras de geração de dados que devem ser tratadas antes de modelagem estatística.
}
\seealso{
\code{\link{sby_profile_data_set}}, \code{\link{sby_profile_data_catalog}}
}
