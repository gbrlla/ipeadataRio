\name{GENERICAverif}

\alias{GENERICAverif}

\title{
Planilha Generica para verificacao
}

\description{
Retorna um \code{dataframe} com a planilha Generica contendo as series requisitadas.
}

\usage{
GENERICAverif(nomes)
}

\arguments{
  \item{nomes}{
  vetor com o \code{sercodigotroll} das series requisitadas.
}
}

\author{Luiz Eduardo Gomes, \email{luiz.gomes@ipea.gov.br} ou
  \email{gomes.leduardo@gmail.com}.
}

\examples{
## Busca de serie unica
CDI <- GENERICAverif(nomes = "SGS366_CDI")

## Busca de multiplas series
ICMS <- GENERICAverif(nomes = c("CONFAZ12_ICMSMG12",
                                "CONFAZ12_ICMSRJ12",
                                "CONFAZ12_ICMSSP12"))
