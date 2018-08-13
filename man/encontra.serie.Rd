\name{encontra.serie}

\alias{encontra.serie}

\title{
Busca de metadados
}

\description{
Retorna um \code{dataframe} com metadados resumidos das series requisitadas juntamente com o grafico dinamico (opcional).
}

\usage{
encontra.serie(serie, plotar = TRUE)
}

\arguments{
  \item{serie}{
  vetor com o \code{sercodigotroll} das series desejadas, nome do banco, \code{serperid} ou \code{serstatus} (pode demandar tempo).
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
## Busca de serie unica por sercodigotroll, exibindo grafico
CDI <- encontra.serie(serie = ("SGS366_CDI"))

## Busca de multiplas series por sercodigotroll, nao exibindo grafico
series1 <- encontra.serie(serie = c("gm12_DOW12","ABATE12_ABPENO12","MTE12_SALMIN12"),
                          plotar = FALSE)

## Busca de multiplas series por sercodigotroll, exibindo grafico
series2 <- encontra.serie(serie = c("ABATE12_ABQUBO12","ABATE12_ABQUBV12"))

## Busca de multiplas series por periodicidade
series3 <- encontra.serie(serie = c("diaria","mensal"))

## Busca mista
series3 <- encontra.serie(serie = c("diaria","MTE12_SALMIN12"))
