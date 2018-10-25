# --------------------------------------------------------- #
# |   INSTITUTO DE PESQUISA ECONOMICA APLICADA - IPEA     | #
# |                    PROJETO IPEADATA                   | #
# --------------------------------------------------------- #
# |   COORDENADOR: ERIVELTON P. GUEDES                    | #
# --------------------------------------------------------- #
# |   PROGRAMADOR: LUIZ EDUARDO S. GOMES                  | #
# --------------------------------------------------------- #
# |   DEPIS - WEBSCRAPPING                                | #
# --------------------------------------------------------- #

#' @title DEPIS - \emph{Webscrapping}
#'
#' @description Realiza a raspagem de dados referentes a algumas series do banco DEPIS a partir da
#' API do SIDRA (\url{https://sidra.ibge.gov.br}).
#'
#' @param gerarGen Logico. Se \code{gerarGen = TRUE}, a planilha \code{GENERICA} e
#' atualizada no diretorio especifico do \emph{ETL}. O \emph{default} e \code{TRUE}.
#'
#' @author Luiz Eduardo Gomes, \email{luiz.gomes@@ipea.gov.br} ou \email{gomes.leduardo@@gmail.com}.
#'
#' @examples
#' #------ Exportando a planilha GENERICA no diretorio.
#' depis <- DEPISwb()
#'
#' @export
#'
#' @importFrom stats na.exclude spline

DEPISwb <- function(gerarGen = TRUE)
{
  # WEBSCRAPPING ----------------------------------------------

  # ------ Codigo das series
  serinput <- c("DEPIS_POPH","DEPIS_POPM","DEPIS_POPRURAL","DEPIS_POPURB")

  # ------ URL api
  url.sidra <- c("http://api.sidra.ibge.gov.br/values/t/202/n1/all/v/allxp/p/all/c2/4/c1/0",
                 "http://api.sidra.ibge.gov.br/values/t/202/n1/all/v/allxp/p/all/c2/5/c1/0",
                 "http://api.sidra.ibge.gov.br/values/t/202/n1/all/v/allxp/p/all/c2/0/c1/2",
                 "http://api.sidra.ibge.gov.br/values/t/202/n1/all/v/allxp/p/all/c2/0/c1/1")

  # ------ Proxy Ipea
  Sys.setenv(http_proxy="http://cache-rj.ipea.gov.br:3128")

  GENERICA <- data.frame(VALDATA = seq(from = as.Date("1970-01-01"),
                                       to = as.Date(paste0(substr(Sys.Date(), 1, 4),"-01-01")),
                                       by = "1 year"))
  for (i in 1:length(serinput))
  {
    # ------ Parametros de controle
    tabela <- NULL
    erro_tabela <- TRUE

    # ------ Carrega a tabela via API
    while (erro_tabela == T)
    {
      Sys.sleep(.01)
      tryCatch({tabela <- array(rjson::fromJSON(RCurl::getURL(url.sidra[i],
                                                              ssl.verifyhost = FALSE,
                                                              ssl.verifypeer = FALSE)))
      erro_tabela <- ifelse(test = is.null(tabela),
                            yes = TRUE,no = FALSE)
      }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
    }

    # ------ Criando DF a partir da tabela
    aux <- data.frame(NULL)
    for (l in 2:length(tabela))
    {
      aux[(l-1),1] <- tabela[[l]]$D3N
      aux[(l-1),2] <- ifelse(test = tabela[[l]]$V == "-",
                             yes = NA, no = as.numeric(tabela[[l]]$V))
    }

    # ------ Ajusta as datas
    aux[,1] <- as.Date(paste0(aux[,1],"-01-01"))
    names(aux)[1] <- "VALDATA"

    # ------ Interpolando as datas
    DFaux <- data.frame(VALDATA = seq(from = min(aux[,1]), to = max(aux[,1]), by = "1 year"))

    # ------ Splines
    spl <- stats::spline(x = as.numeric(aux[,1]),
                         y = aux[,2],
                         n = as.numeric(max(aux[,1]) - min(aux[,1])) + 1)

    # ------ Selecionando os valores
    val <- NULL
    for (k in 1:length(as.numeric(DFaux$VALDATA))){val <- c(val, spl$y[which(spl$x == as.numeric(DFaux$VALDATA)[k])])}
    DFaux <- data.frame(DFaux, val)
    names(DFaux)[2] <- serinput[i]

    # ------ Generica
    GENERICA <- merge(x = GENERICA, y = DFaux, by = "VALDATA", all = TRUE)
  }

  # ------ Generica
  GENERICA <- na.exclude(GENERICA)
  GENERICA$VALDATA <- GENERICA$VALDATA + 14

  # ------ Comparando valores
  gen <- ipeadataRio::genericaVerif(serinput)
  r <- 1
  for(i in 1:length(serinput)){r <- c(r, which(names(gen) == serinput[i]))}
  gen <- gen[,r]
  gen <- na.exclude(gen)
  gen$VALDATA <- paste0(substr(gen$VALDATA, 1, 4),"-01-15")

  # ------ Atualizar?
  atualizar <- FALSE
  if(max(GENERICA$VALDATA) > max(gen$VALDATA)){atualizar <- TRUE}

  if(gerarGen & atualizar)
  {
    # SALVANDO GENERICA --------------------------------------

    # ------ Exportando xls
    xlsx::write.xlsx(x = GENERICA,
                     file = file.path("","","Srjn3","area_corporativa","Projeto_IPEADATA","ETL","Generica","DEPISger.xls"),
                     sheetName="Generica", row.names = FALSE, showNA = FALSE)
  }

  # ATUALIZANDO AUTOLOG --------------------------------------

  # ------ Lendo autolog
  autolog <- utils::read.csv2(file = file.path("","","Srjn3","area_corporativa","Projeto_IPEADATA","Geral","PacoteIpeadataRio","autolog.csv"))

  # ------ Editando estrutura
  autolog$data.hora <- as.character(autolog$data.hora)
  autolog$usuario <- as.character(autolog$usuario)
  autolog$acao <- as.character(autolog$acao)

  # ------ Atualizando com credenciais
  r <- nrow(autolog) + 1
  autolog[r,] <- c(as.character(Sys.time()),Sys.getenv("USERNAME"),"DEPISger")

  # ------ Ordenando
  autolog <- autolog[order(x = autolog$data.hora,decreasing = TRUE),]

  # ------ Salvando autolog
  utils::write.csv2(x = autolog,
                    file = file.path("","","Srjn3","area_corporativa","Projeto_IPEADATA","Geral","PacoteIpeadataRio","autolog.csv"),
                    row.names = FALSE)

  # ------ Resultado
  return(GENERICA)
}
