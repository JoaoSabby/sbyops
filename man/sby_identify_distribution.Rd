% Generated manually for the development snapshot.
\name{sby_identify_distribution}
\alias{sby_identify_distribution}
\title{Identificar distribuições contínuas candidatas}
\usage{
sby_identify_distribution(
  x,
  candidates = sby_distribution_candidates(),
  max_sample_size = 100000L,
  tail_probability = 0.01,
  model_prior = NULL,
  num_treads = NULL
)
}
\arguments{
\item{x}{Vetor numérico. Classes de data e data-hora não são aceitas.}
\item{candidates}{Tabela candidata produzida por \code{sby_distribution_candidates()} ou estrutura compatível.}
\item{max_sample_size}{Inteiro positivo ou \code{Inf}. Vetores maiores são amostrados de forma determinística.}
\item{tail_probability}{Probabilidade usada para sinalizar observações em caudas ajustadas.}
\item{model_prior}{Vetor numérico nomeado opcional com pesos prévios positivos para modelos.}
\item{num_treads}{Inteiro positivo opcional com limite temporário de threads para a chamada.}
}
\value{
Um tibble com uma linha por distribuição candidata, incluindo convergência, log-verossimilhança, AIC, AICc, BIC, estatísticas de aderência, pesos de Akaike, aproximação posterior por BIC e mensagens de erro ou aviso.
}
\description{
Identifica distribuições contínuas candidatas para um vetor numérico por ajuste de máxima verossimilhança, critérios de informação e diagnósticos empíricos de aderência.
}
\details{
Para cada família compatível com o suporte observado, a rotina ajusta um modelo GAMLSS, calcula \eqn{\ell(\hat\theta)}, AIC, AICc, BIC, pesos de Akaike e probabilidades posteriores aproximadas via BIC. As estatísticas de Kolmogorov--Smirnov, Cramér--von Mises e Anderson--Darling são medidas de ranqueamento, sem valores-p ingênuos, porque os parâmetros são estimados nos mesmos dados.

A aproximação bayesiana é condicional ao conjunto de modelos, aos ajustes convergentes e aos pesos prévios fornecidos. Amostras muito longas são reduzidas por amostragem determinística para preservar reprodutibilidade, velocidade e consumo previsível de memória. Os pacotes \pkg{gamlss} e \pkg{gamlss.dist} são dependências opcionais carregadas em tempo de execução.
}
\references{
Akaike, H. (1974). A new look at the statistical model identification. \emph{IEEE Transactions on Automatic Control}, 19(6), 716--723.

Schwarz, G. (1978). Estimating the dimension of a model. \emph{The Annals of Statistics}, 6(2), 461--464.

Rigby, R. A.; Stasinopoulos, D. M. (2005). Generalized additive models for location, scale and shape. \emph{Applied Statistics}, 54(3), 507--554.

Burnham, K. P.; Anderson, D. R. (2002). \emph{Model Selection and Multimodel Inference}. Springer.
}
\seealso{
\code{\link{sby_distribution_candidates}}
}
