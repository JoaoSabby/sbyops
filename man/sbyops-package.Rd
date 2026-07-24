% Generated manually for the development snapshot.
\docType{package}
\name{sbyops-package}
\alias{sbyops}
\alias{sbyops-package}
\title{sbyops: Ferramentas computacionais para dados tabulares}
\description{
Pacote de ferramentas composáveis para seleção de colunas, escrita Parquet, perfilamento estatístico, diagnóstico de relações, identificação de distribuições e detecção robusta de outliers em dados tabulares.
}
\details{
O pacote é organizado por famílias de ferramentas. A API pública usa prefixo \code{sby_}, aceita entradas tabulares usuais e preserva contexto de threads durante rotinas intensivas. Novas ferramentas devem ser adicionadas em módulos coerentes, com documentação de ajuda própria e sem efeitos colaterais permanentes na sessão do usuário.
}
