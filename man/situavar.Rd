\name{situavar}

\alias{situavar}

\title{
Situacao das series
}
\description{
Retorna um \code{dataframe} com metadados resumidos acerca do atraso na atualizacao das series requisitadas.
}
\usage{
situavar(serie, exportar = TRUE, saida.aux = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{serie}{
  vetor com o \code{sercodigotroll} das series requisitadas, nome do banco, \code{serperid} ou \code{serstatus} (pode demandar tempo).
}

  \item{exportar}{
  logico. Se \code{exportar = TRUE}, o relatorio e exportado em formato \code{.xls}.
}

  \item{saida.aux}{
  logico. \code{saida.aux = TRUE} e utilizado internamente para construcao do indice de atraso.
}
}

\details{
Os relatorios sao exportados para a pasta localizada em \code{\\\Srjn3\area_corporativa\Projeto_IPEADATA\Geral\PacoteIpeadataRio\situavar}
}

\author{Luiz Eduardo Gomes, \email{luiz.gomes@ipea.gov.br} ou
  \email{gomes.leduardo@gmail.com}.
}

\examples{
## Busca de unico banco
sit1 <- situavar(serie = ("CONFAZ12"))

## Busca de multiplos bancos
sit2 <- situavar(serie = c("IGP12","SECEX12"))
sit3 <- situavar(serie = c("PIMPFN12","ANDIMA12"), exportar = FALSE) # nao exportando

## Busca de multiplos bancos com diferentes periodicidades
sit4 <- situavar(serie = c("PIMPFN12","GM366","ANDIMA4"))

## Busca de multiplas series por sercodigotroll
sit5 <- situavar(serie = c("ABATE12_ABQUBO12","ABATE12_ABQUBV12"))

## Busca de multiplas series por periodicidade
sit6 <- situavar(serie = c("diaria","mEnSaL"))
sit7 <- situavar(serie = ("ANUAL"))

## Busca mista
sit8 <- situavar(serie = c("diaria","MTE12_SALMIN12"), exportar = FALSE) # nao exportando
