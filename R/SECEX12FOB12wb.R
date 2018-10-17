# --------------------------------------------------------- #
# |   INSTITUTO DE PESQUISA ECONOMICA APLICADA - IPEA     | #
# |                    PROJETO IPEADATA                   | #
# --------------------------------------------------------- #
# |   COORDENADOR: ERIVELTON P. GUEDES                    | #
# --------------------------------------------------------- #
# |   PROGRAMADOR: LUIZ EDUARDO S. GOMES                  | #
# --------------------------------------------------------- #
# |   SECEX12 VIA SQL                                     | #
# --------------------------------------------------------- #

#' @title Codigos dos paises - SECEX MDIC
#'
#' @description Conjunto de dados contendo os codigos dos paises utilizado pela SECEX/MDIC.
#'
#' @format Banco de dados com 278 observacao(oes) e 3 variavel(is):
#' \describe{
#'   \item{"CO_PAIS"}{Codigo dos pais}
#'   \item{"NO_PAIS_ING"}{Nome dos pais (ingles)}
#'   \item{"TERRIT_PAISES"}{Codigo de territorio (Numerico)}
#' }
#' @source \url{http://www.mdic.gov.br/arquivos/dwnl_1362158650.pdf}

"codpaises.SECEX12FOB12"

#' @title Lista dos paises - MERCOSUL - SECEX MDIC
#'
#' @description Conjunto de dados contendo os codigos dos paises do MERCOSUL utilizado pela SECEX/MDIC.
#'
#' @format Banco de dados com 468 observacao(oes) e 8 variavel(is):
#' \describe{
#'   \item{"Datas"}{Datas}
#'   \item{"cod_membro_1"}{Codigo do membro 1}
#'   \item{"cod_membro_2"}{Codigo do membro 2}
#'   \item{"cod_membro_3"}{Codigo do membro 3}
#'   \item{"cod_membro_4"}{Codigo do membro 4}
#'   \item{"cod_membro_5"}{Codigo do membro 5}
#'   \item{"cod_membro_6"}{Codigo do membro 6}
#'   \item{"cod_membro_7"}{Codigo do membro 7}
#' }
#'
#' @source \url{https://pt.wikipedia.org/wiki/Mercado_Comum_do_Sul#Membros}

"listadepaises.Mercosul"

#' @title Lista dos paises - ZONA DO EURO - SECEX MDIC
#'
#' @description Conjunto de dados contendo os codigos dos paises da Zona do Euro utilizado pela SECEX/MDIC.
#'
#' @format Banco de dados com 39 observacao(oes) e 21 variavel(is):
#' \describe{
#'   \item{"Datas"}{Datas}
#'   \item{"cod_membro_1"}{Codigo do membro 1}
#'   \item{"cod_membro_2"}{Codigo do membro 2}
#'   \item{"cod_membro_3"}{Codigo do membro 3}
#'   \item{"cod_membro_4"}{Codigo do membro 4}
#'   \item{"cod_membro_5"}{Codigo do membro 5}
#'   \item{"cod_membro_6"}{Codigo do membro 6}
#'   \item{"cod_membro_7"}{Codigo do membro 7}
#'   \item{"cod_membro_8"}{Codigo do membro 8}
#'   \item{"cod_membro_9"}{Codigo do membro 9}
#'   \item{"cod_membro_10"}{Codigo do membro 10}
#'   \item{"cod_membro_11"}{Codigo do membro 11}
#'   \item{"cod_membro_12"}{Codigo do membro 12}
#'   \item{"cod_membro_13"}{Codigo do membro 13}
#'   \item{"cod_membro_14"}{Codigo do membro 14}
#'   \item{"cod_membro_15"}{Codigo do membro 15}
#'   \item{"cod_membro_16"}{Codigo do membro 16}
#'   \item{"cod_membro_17"}{Codigo do membro 17}
#'   \item{"cod_membro_18"}{Codigo do membro 18}
#'   \item{"cod_membro_19"}{Codigo do membro 19}
#'   \item{"cod_membro_20"}{Codigo do membro 20}
#' }
#'
#' @source \url{https://pt.wikipedia.org/wiki/Zona_Euro#Membros}

"listadepaises.ZE"

#' @title SECEX12FOB12 via SQL
#'
#' @description Realiza a ordenacao e organizacao dos dados referentes ao
#' banco SECEX12 (FOB12) a partir do banco SQL interno e exporta a planilha \code{GENERICA}
#' no diretorio especifico do ETL.
#'
#' @param gerarGen Logico. Se \code{gerarGen = TRUE}, a planilha \code{GENERICA} e
#' atualizada no diretorio especifico do \emph{ETL}. O \emph{default} e \code{TRUE}.
#'
#' @param completa Logico. Se \code{completa = FALSE}, e requisitado apenas os ultimos
#' 10 anos da serie temporal. Se \code{completa = TRUE}, a serie temporal completa e requisitada,
#' demandando tempo. O \emph{default} e \code{FALSE}.
#'
#' @author Luiz Eduardo Gomes, \email{luiz.gomes@@ipea.gov.br} ou \email{gomes.leduardo@@gmail.com}.
#'
#' @note Quando nao ha informacao disponivel, o valor 0 (zero) e atribuido.
#'
#' @examples
#' #------ Exportando a planilha GENERICA no diretorio.
#' fob1 <- SECEX12FOB12wb()
#'
#' #------ Nao exportando. (Indicado para quem nao possui acesso a pasta do ETL)
#' fob2 <- SECEX12FOB12wb(gerarGen = FALSE)
#'
#' @export

SECEX12FOB12wb <- function(gerarGen = TRUE, completa = FALSE)
{
  # LENDOS DADOS --------------------------------------

  # ------ Texto informativo
  message("Requisitando dados via PostgreSQL")

  # ------ Codigo dos paises (DB interno)
  codpaises <- ipeadataRio::codpaises.SECEX12FOB12$CO_PAIS

  # ------ Abrindo conexao
  conAccess <- utils::read.csv2(file.path("","","Srjn3","area_corporativa","Projeto_IPEADATA","Geral","PacoteIpeadataRio","conPostgreSQL.csv"))

  con <- RPostgreSQL::dbConnect(drv = as.character(conAccess$drv),
                                dbname = as.character(conAccess$dbname),
                                host = as.character(conAccess$host),
                                port = conAccess$port,
                                user = as.character(conAccess$user),
                                password = as.character(conAccess$password))

  # ------ Requerendo dados pelo SQL
  paises_exp <- RPostgreSQL::dbGetQuery(con,
                                        paste0("SELECT num_ano,num_mes,
                                               SUM(num_valor_fob) AS valor, cod_pais
                                               FROM public.vw_comercio_exterior_urf
                                               WHERE imp_exp='e' AND num_ano>=",
                                               ifelse(completa,1990,(as.POSIXlt(Sys.Date()))$year+1900-10),
                                               "AND cod_pais IN (",
                                               paste0(codpaises,
                                                      collapse = ", "),")
                                               GROUP BY num_ano,num_mes,
                                               cod_pais,nme_pais;"))

  paises_imp <- RPostgreSQL::dbGetQuery(con,
                                        paste0("SELECT num_ano,num_mes,
                                               SUM(num_valor_fob) AS valor, cod_pais
                                               FROM public.vw_comercio_exterior_urf
                                               WHERE imp_exp='i' AND num_ano>=",
                                               ifelse(completa,1990,(as.POSIXlt(Sys.Date()))$year+1900-10),
                                               "AND cod_pais IN (",
                                               paste0(codpaises,
                                                      collapse = ", "),")
                                               GROUP BY num_ano,num_mes,
                                               cod_pais,nme_pais;"))

  # ------ Fechando conexao
  RPostgreSQL::dbDisconnect(conn = con)

  # ORGANIZANDO --------------------------------------

  # ------ Texto informativo
  message("Configurando planilha de atualizacao")

  exportacao <- data.frame(SERCODIGO = paste0("SECEX12_X",ifelse(nchar(as.character(paises_exp$cod_pais)) == 1,
                                                                 paste0("00",paises_exp$cod_pais),
                                                                 ifelse(nchar(as.character(paises_exp$cod_pais)) == 2,
                                                                        paste0("0",paises_exp$cod_pais),
                                                                        paises_exp$cod_pais)),
                                              "FOB12"),
                           VALDATA = as.Date(paste0(paises_exp$num_ano,"-",paises_exp$num_mes,"-15")),
                           VALVALOR = paises_exp$valor)
  exportacao <- exportacao[order(exportacao$SERCODIGO,exportacao$VALDATA),]

  importacao <- data.frame(SERCODIGO = paste0("SECEX12_M",ifelse(nchar(as.character(paises_imp$cod_pais)) == 1,
                                                                 paste0("00",paises_imp$cod_pais),
                                                                 ifelse(nchar(as.character(paises_imp$cod_pais)) == 2,
                                                                        paste0("0",paises_imp$cod_pais),
                                                                        paises_imp$cod_pais)),
                                              "FOB12"),
                           VALDATA = as.Date(paste0(paises_imp$num_ano,"-",paises_imp$num_mes,"-15")),
                           VALVALOR = paises_imp$valor)
  importacao <- importacao[order(importacao$SERCODIGO,importacao$VALDATA),]

  # GENERICA --------------------------------------

  GENERICA_EXP <- data.frame(VALDATA = sort(unique(exportacao$VALDATA)))
  for (i in 1:length(unique(exportacao$SERCODIGO)))
  {
    GENERICA_EXP <- merge(GENERICA_EXP,
                          subset(exportacao,
                                 exportacao$SERCODIGO == as.character(unique(exportacao$SERCODIGO))[i])[,-1],
                          by = "VALDATA", all = TRUE)
    names(GENERICA_EXP)[i+1] <- as.character(unique(exportacao$SERCODIGO))[i]
  }

  GENERICA_IMP <- data.frame(VALDATA = sort(unique(importacao$VALDATA)))
  for (i in 1:length(unique(importacao$SERCODIGO)))
  {
    GENERICA_IMP <- merge(GENERICA_IMP,
                          subset(importacao,
                                 importacao$SERCODIGO == as.character(unique(importacao$SERCODIGO))[i])[,-1],
                          by = "VALDATA", all = TRUE)
    names(GENERICA_IMP)[i+1] <- as.character(unique(importacao$SERCODIGO))[i]
  }

  # ------ Zerando os NAs (Faz sentido no caso de importacao e exportacao)
  for (i in 2:ncol(GENERICA_EXP)){GENERICA_EXP[,i] <- ifelse(is.na(GENERICA_EXP[,i]),0,GENERICA_EXP[,i])}
  for (i in 2:ncol(GENERICA_IMP)){GENERICA_IMP[,i] <- ifelse(is.na(GENERICA_IMP[,i]),0,GENERICA_IMP[,i])}

  # ------ Eliminando objetos
  rm(codpaises,conAccess,con,i,paises_exp,paises_imp,exportacao,importacao)

  # GENERICA (BLOCOS ECONOMICOS, DIVISOES E CONTINENTES) --------------------------------------
  GENERICA_NEW <- data.frame(VALDATA = GENERICA_IMP$VALDATA)

  # ------ Continentes e Oriente Medio
  for (k in 0:6)
  {
    # ------ Importacao ###
    # ------ Selecionando regiao
    codpaises.temp <- subset(x = ipeadataRio::codpaises.SECEX12FOB12,
                             subset = ipeadataRio::codpaises.SECEX12FOB12$TERRIT_PAISES == k)

    # ------ Completando nomes da variavel
    codpaises.temp$CO_PAIS <- paste0("SECEX12_M",ifelse(nchar(as.character(codpaises.temp$CO_PAIS)) == 1,
                                                        paste0("00", codpaises.temp$CO_PAIS),
                                                        ifelse(nchar(as.character(codpaises.temp$CO_PAIS)) == 2,
                                                               paste0("0", codpaises.temp$CO_PAIS),
                                                               codpaises.temp$CO_PAIS)),
                                     "FOB12")

    # ------ Selecionando as colunas pertencentes a regiao corrente
    rr <- NULL
    for (l in 1:length(codpaises.temp$CO_PAIS)){rr <-c(rr, which(names(GENERICA_IMP) == codpaises.temp$CO_PAIS[l]))}

    # ------ Juntando ao novo DF
    GENERICA_NEW <- merge(x = GENERICA_NEW, y = data.frame(VALDATA = GENERICA_IMP$VALDATA,
                                                           rowSums(GENERICA_IMP[,rr])),
                          by = "VALDATA",
                          all = TRUE)
    names(GENERICA_NEW)[ncol(GENERICA_NEW)] <- paste0("M", k)

    # ------ Exportacao ###
    # ------ Selecionando regiao
    codpaises.temp <- subset(x = ipeadataRio::codpaises.SECEX12FOB12,
                             subset = ipeadataRio::codpaises.SECEX12FOB12$TERRIT_PAISES == k)

    # ------ Completando nomes da variavel
    codpaises.temp$CO_PAIS <- paste0("SECEX12_X",ifelse(nchar(as.character(codpaises.temp$CO_PAIS)) == 1,
                                                        paste0("00", codpaises.temp$CO_PAIS),
                                                        ifelse(nchar(as.character(codpaises.temp$CO_PAIS)) == 2,
                                                               paste0("0", codpaises.temp$CO_PAIS),
                                                               codpaises.temp$CO_PAIS)),
                                     "FOB12")

    # ------ Selecionando as colunas pertencentes a regiao corrente
    rr <- NULL
    for (l in 1:length(codpaises.temp$CO_PAIS)){rr <-c(rr, which(names(GENERICA_EXP) == codpaises.temp$CO_PAIS[l]))}

    # ------ Juntando ao novo DF
    GENERICA_NEW <- merge(x = GENERICA_NEW, y = data.frame(VALDATA = GENERICA_EXP$VALDATA,
                                                           rowSums(GENERICA_EXP[,rr])),
                          by = "VALDATA",
                          all = TRUE)
    names(GENERICA_NEW)[ncol(GENERICA_NEW)] <- paste0("X", k)
  }

  # ------ Nomeando
  names(GENERICA_NEW)[-1] <- c("SECEX12_IMPORTETC12",
                               "SECEX12_EXPORTETC12",
                               "SECEX12_IMPORTAMERICAN12",
                               "SECEX12_EXPORTAMERICAN12",
                               "SECEX12_IMPORTAMERICAS12",
                               "SECEX12_EXPORTAMERICAS12",
                               "SECEX12_IMPORTAFRICA12",
                               "SECEX12_EXPORTAFRICA12",
                               "SECEX12_IMPORTEUROPA12",
                               "SECEX12_EXPORTEUROPA12",
                               "SECEX12_IMPORTASIA12",
                               "SECEX12_EXPORTASIA12",
                               "SECEX12_IMPORTOMEDIO12",
                               "SECEX12_EXPORTOMEDIO12")

  # ------ MERCOSUL (importacao) ###
  # ------ Organizando datas (sempre dia 15)
  codpaises.temp <- ipeadataRio::listadepaises.Mercosul
  codpaises.temp$Datas <- codpaises.temp$Datas + 14

  mercosul_aux <- NULL
  for (l in 1:length(codpaises.temp$Datas))
  {
    # ------ Selecionando a data corrente
    aux <- GENERICA_IMP[which(GENERICA_IMP$VALDATA == codpaises.temp$Datas[l]),]

    if(nrow(aux) > 0)
    {
      # ------ Completando nomes da variavel
      cod_paises <- as.numeric(na.exclude(as.numeric(codpaises.temp[l,-1])))
      cod_paises <- paste0("SECEX12_M",ifelse(nchar(as.character(cod_paises)) == 1,
                                              paste0("00", cod_paises),
                                              ifelse(nchar(as.character(cod_paises)) == 2,
                                                     paste0("0", cod_paises),
                                                     cod_paises)),
                           "FOB12")

      # ------ Selecionando as colunas pertencentes a regiao corrente
      rr <- NULL
      for (k in 1:length(cod_paises)){rr <-c(rr, which(names(aux) == cod_paises[k]))}
      mercosul_aux <-c(mercosul_aux ,sum(aux[,rr]))
    }
  }

  # ------ Juntando ao novo DF
  GENERICA_NEW[,ncol(GENERICA_NEW) + 1] <- mercosul_aux
  names(GENERICA_NEW)[ncol(GENERICA_NEW)] <- "SECEX12_IMPORTMERCOSUL12"

  # ------ MERCOSUL (exportacao) ###
  mercosul_aux <- NULL
  for (l in 1:length(codpaises.temp$Datas))
  {
    # ------ Selecionando a data corrente
    aux <- GENERICA_EXP[which(GENERICA_EXP$VALDATA == codpaises.temp$Datas[l]),]

    if(nrow(aux) > 0)
    {
      # ------ Completando nomes da variavel
      cod_paises <- as.numeric(na.exclude(as.numeric(codpaises.temp[l,-1])))
      cod_paises <- paste0("SECEX12_X",ifelse(nchar(as.character(cod_paises)) == 1,
                                              paste0("00", cod_paises),
                                              ifelse(nchar(as.character(cod_paises)) == 2,
                                                     paste0("0", cod_paises),
                                                     cod_paises)),
                           "FOB12")
      rr <- NULL
      for (k in 1:length(cod_paises)){rr <-c(rr, which(names(aux) == cod_paises[k]))}
      mercosul_aux <-c(mercosul_aux ,sum(aux[,rr]))
    }
  }

  GENERICA_NEW[,ncol(GENERICA_NEW) + 1] <- mercosul_aux
  names(GENERICA_NEW)[ncol(GENERICA_NEW)] <- "SECEX12_EXPORTMERCOSUL12"
  rm(mercosul_aux)

  # ------ Zona do Euro (importacao)
  codpaises.temp <- NULL
  for (l in sort(rep(1:nrow(ipeadataRio::listadepaises.ZE), 12)))
  {
    codpaises.temp <- rbind(codpaises.temp,ipeadataRio::listadepaises.ZE[l,])
  }

  codpaises.temp$Datas <- seq(from = as.Date(paste0(ipeadataRio::listadepaises.ZE[1,1],"-01-15")),
                              to = as.Date(paste0(ipeadataRio::listadepaises.ZE[nrow(ipeadataRio::listadepaises.ZE),1],
                                                  "-12-15")),
                              by = "1 month")

  ZE_aux <- NULL

  for (l in 1:length(codpaises.temp$Datas))
  {
    aux <- GENERICA_IMP[which(GENERICA_IMP$VALDATA == codpaises.temp$Datas[l]),]

    if(nrow(aux) > 0)
    {
      cod_paises <- as.numeric(na.exclude(as.numeric(codpaises.temp[l,-1])))
      cod_paises <- paste0("SECEX12_M",ifelse(nchar(as.character(cod_paises)) == 1,
                                              paste0("00", cod_paises),
                                              ifelse(nchar(as.character(cod_paises)) == 2,
                                                     paste0("0", cod_paises),
                                                     cod_paises)),
                           "FOB12")
      rr <- NULL
      for (k in 1:length(cod_paises)){rr <-c(rr, which(names(aux) == cod_paises[k]))}
      ZE_aux <-c(ZE_aux ,sum(aux[,rr]))
    }
  }

  GENERICA_NEW[,ncol(GENERICA_NEW) + 1] <- ZE_aux
  names(GENERICA_NEW)[ncol(GENERICA_NEW)] <- "SECEX12_IMPORTZE12"

  # ------ MERCOSUL (exportacao)
  ZE_aux <- NULL

  for (l in 1:length(codpaises.temp$Datas))
  {
    aux <- GENERICA_EXP[which(GENERICA_EXP$VALDATA == codpaises.temp$Datas[l]),]

    if(nrow(aux) > 0)
    {
      cod_paises <- as.numeric(na.exclude(as.numeric(codpaises.temp[l,-1])))
      cod_paises <- paste0("SECEX12_X",ifelse(nchar(as.character(cod_paises)) == 1,
                                              paste0("00", cod_paises),
                                              ifelse(nchar(as.character(cod_paises)) == 2,
                                                     paste0("0", cod_paises),
                                                     cod_paises)),
                           "FOB12")
      rr <- NULL
      for (k in 1:length(cod_paises)){rr <-c(rr, which(names(aux) == cod_paises[k]))}
      ZE_aux <-c(ZE_aux ,sum(aux[,rr]))
    }
  }

  GENERICA_NEW[,ncol(GENERICA_NEW) + 1] <- ZE_aux
  names(GENERICA_NEW)[ncol(GENERICA_NEW)] <- "SECEX12_EXPORTZE12"
  rm(ZE_aux)

  # ------ Comparando valores
  VALORES.BASE_EXP <- genericaVerif(nomes = names(GENERICA_EXP)[-1])
  VALORES.BASE_IMP <- genericaVerif(nomes = names(GENERICA_IMP)[-1])

  # ------ Organizando data
  VALORES.BASE_EXP$VALDATA <- VALORES.BASE_EXP$VALDATA + 14
  VALORES.BASE_IMP$VALDATA <- VALORES.BASE_IMP$VALDATA + 14

  # ------ Base auxiliar
  VALORES.BASE2_EXP <- merge(x = VALORES.BASE_EXP,y = GENERICA_EXP,by = "VALDATA")[,1:ncol(GENERICA_EXP)]
  VALORES.BASE2_IMP <- merge(x = VALORES.BASE_IMP,y = GENERICA_IMP,by = "VALDATA")[,1:ncol(GENERICA_IMP)]

  VALORES.BASE3_EXP <- merge(x = GENERICA_EXP,y = VALORES.BASE_EXP,by = "VALDATA")[,1:ncol(GENERICA_EXP)]
  VALORES.BASE3_IMP <- merge(x = GENERICA_IMP,y = VALORES.BASE_IMP,by = "VALDATA")[,1:ncol(GENERICA_IMP)]

  # ------ Atualizar?
  atualizar <- FALSE
  if((nrow(GENERICA_EXP) > nrow(VALORES.BASE2_EXP))|
     (nrow(GENERICA_IMP) > nrow(VALORES.BASE2_IMP))){atualizar <- TRUE}
  if((nrow(GENERICA_EXP) == nrow(VALORES.BASE2_EXP)))
  {
    if(sum(VALORES.BASE2_EXP[,-1] != GENERICA_EXP[,-1],na.rm = TRUE) > 0){atualizar <- TRUE}
  }
  if((nrow(GENERICA_IMP) == nrow(VALORES.BASE2_IMP)))
  {
    if(sum(VALORES.BASE2_IMP[,-1] != GENERICA_IMP[,-1],na.rm = TRUE) > 0){atualizar <- TRUE}
  }

  if(gerarGen & atualizar)
  {
    # SALVANDO GENERICA --------------------------------------

    # ------ Texto informativo
    message(paste("Exportando planilha de atualizacao para",
                  file.path("","","Srjn3","area_corporativa","Projeto_IPEADATA","ETL","Generica")))

    # ------ Exportando xls
    xlsx::write.xlsx(x = GENERICA_EXP[,c(1,2:150)],
                     file = file.path("","","Srjn3","area_corporativa","Projeto_IPEADATA","ETL","Generica","SECEX12EXP1_Generica.xls"),
                     sheetName="Generica", row.names = FALSE, showNA = FALSE)

    xlsx::write.xlsx(x = GENERICA_EXP[,c(1,151:ncol(GENERICA_EXP))],
                     file = file.path("","","Srjn3","area_corporativa","Projeto_IPEADATA","ETL","Generica","SECEX12EXP2_Generica.xls"),
                     sheetName="Generica", row.names = FALSE, showNA = FALSE)

    xlsx::write.xlsx(x = GENERICA_IMP[,c(1,2:150)],
                     file = file.path("","","Srjn3","area_corporativa","Projeto_IPEADATA","ETL","Generica","SECEX12IMP1_Generica.xls"),
                     sheetName="Generica", row.names = FALSE, showNA = FALSE)

    xlsx::write.xlsx(x = GENERICA_IMP[,c(1,151:ncol(GENERICA_IMP))],
                     file = file.path("","","Srjn3","area_corporativa","Projeto_IPEADATA","ETL","Generica","SECEX12IMP2_Generica.xls"),
                     sheetName="Generica", row.names = FALSE, showNA = FALSE)

    xlsx::write.xlsx(x = GENERICA_NEW,
                     file = file.path("","","Srjn3","area_corporativa","Projeto_IPEADATA","ETL","Generica","SECEX12BLOCOS_Generica.xls"),
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
  autolog[r,] <- c(as.character(Sys.time()),Sys.getenv("USERNAME"),"SECEX12_Generica")

  # ------ Ordenando
  autolog <- autolog[order(x = autolog$data.hora,decreasing = TRUE),]

  # ------ Salvando autolog
  utils::write.csv2(x = autolog,
                    file = file.path("","","Srjn3","area_corporativa","Projeto_IPEADATA","Geral","PacoteIpeadataRio","autolog.csv"),
                    row.names = FALSE)

  # ------ Eliminando objetos
  rm(autolog,r)

  # TEXTO RESUMO ----------------------------------------

  cat("\n")
  cat(paste("Relatorio do banco SECEX12 em",Sys.Date(),"\n"))
  cat("RESUMO \n")
  cat(paste("Numero de revisoes ....................",
            sum(VALORES.BASE2_EXP[,-1] != VALORES.BASE3_EXP[,-1],na.rm = TRUE) +
              sum(VALORES.BASE2_IMP[,-1] != VALORES.BASE3_IMP[,-1],na.rm = TRUE)),"\n")
  cat("\n")

  # ------ Resultado
  return(list(SECEX12EXP_Generica = GENERICA_EXP,
              SECEX12IMP_Generica = GENERICA_IMP))

}
