% Generated manually for the development snapshot.
\name{sby_config}
\alias{sby_config}
\title{Configurar limiares e threads do sbyops}
\usage{
sby_config(
  sby_config_start_fortran = 10000L,
  sby_config_start_blas = 100000L,
  sby_config_max_threads = 2L
)
}
\arguments{
\item{sby_config_start_fortran}{Inteiro positivo com o limiar de ativação do backend Fortran.}
\item{sby_config_start_blas}{Inteiro positivo com o limiar de ativação do backend BLAS.}
\item{sby_config_max_threads}{Inteiro positivo com o limite global padrão de threads.}
}
\value{Lista nomeada com os valores validados e gravados em \code{options()}.}
\description{Define opções globais de planejamento de execução usadas pelas ferramentas do pacote para escolher backends e limitar paralelismo.}
\details{A configuração é lida no momento de cada chamada. Funções que recebem \code{num_treads} podem sobrescrever temporariamente \code{sby_config_max_threads} sem alterar a configuração armazenada.}
\examples{sby_config(sby_config_max_threads = 2L)}
