% Generated manually for the development snapshot.
\name{sby_detect_multivariate_outliers}
\alias{sby_detect_multivariate_outliers}
\title{Detectar outliers multivariados com MCD robusto}
\usage{
sby_detect_multivariate_outliers(
  .data,
  ...,
  alpha = 0.001,
  max_fit_rows = 50000L,
  num_treads = NULL
)
}
\arguments{
\item{.data}{Tabela DuckDB, data frame, tibble ou data.table.}
\item{...}{Expressões tidyselect. Quando omitidas, colunas numéricas são selecionadas.}
\item{alpha}{Probabilidade de cauda superior usada no ponto de corte qui-quadrado.}
\item{max_fit_rows}{Tamanho máximo da amostra determinística usada para ajustar o MCD.}
\item{num_treads}{Inteiro positivo opcional com limite temporário de threads para a chamada.}
}
\value{
Um tibble com uma linha por linha de entrada e colunas com número da linha, indicador de completude, distância de Mahalanobis robusta, limite qui-quadrado, indicador de outlier, quantidade de variáveis e quantidade de linhas usadas no ajuste.
}
\description{
Detecta observações atípicas multivariadas por distância de Mahalanobis robusta estimada pelo determinante mínimo da covariância, ou MCD.
}
\details{
Para linhas completas e finitas, a rotina calcula \eqn{D_i^2=(x_i-\hat\mu_R)^\top\hat\Sigma_R^{-1}(x_i-\hat\mu_R)}, em que \eqn{\hat\mu_R} e \eqn{\hat\Sigma_R} são estimadores robustos de localização e dispersão. O ponto de corte é o quantil \eqn{1-\alpha} da distribuição \eqn{\chi^2_p}. Linhas incompletas são mantidas no resultado com estatísticas indefinidas para preservar rastreabilidade.
}
\references{
Rousseeuw, P. J. (1985). Multivariate estimation with high breakdown point. In \emph{Mathematical Statistics and Applications}. Reidel.

Rousseeuw, P. J.; Van Driessen, K. (1999). A fast algorithm for the minimum covariance determinant estimator. \emph{Technometrics}, 41(3), 212--223.

Maronna, R. A.; Martin, R. D.; Yohai, V. J.; Salibián-Barrera, M. (2019). \emph{Robust Statistics: Theory and Methods}. Wiley.
}
