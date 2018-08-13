\name{dados.faltantes}
\alias{dados.faltantes}

\title{
Busca por descontinuidade
}

\description{
Retorna um \code{dataframe} com metadados resumidos acerca da descontinuidade nas series requisitadas.
}

\usage{
dados.faltantes(serie, plotar = TRUE)
}

\arguments{
  \item{serie}{
  vetor com o \code{sercodigotroll} das series requisitadas, nome do banco, \code{serperid} ou \code{serstatus} (pode demandar tempo).
}

  \item{plotar}{
  logico. Se \code{plotar = TRUE}, o grafico dinamico e exibido.
}
}

\details{
Para evitar possiveis distorcoes no grafico, impos-se um limite para a exibicao simultanea de 5 (cinco) series.
}

\author{Luiz Eduardo Gomes, \email{luiz.gomes@ipea.gov.br} ou
  \email{gomes.leduardo@gmail.com}.
}

\examples{
## Busca de unico banco
desc1 <- dados.faltantes(serie = c("CONFAZ12_ICMSAC12"))

## Busca de multiplos bancos
desc2 <- dados.faltantes(serie = c("gm12_DOW12","ABATE12_ABPENO12","MTE12_SALMIN12"))

## Busca de multiplas series por periodicidade, nao exibindo grafico
desc3 <- dados.faltantes(serie = c("mensal"),plotar = FALSE) # Demanda tempo!
