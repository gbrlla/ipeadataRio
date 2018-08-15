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

# CARREGANDO PACOTES ----------------------------------------

pacotes<-c("rjson","RCurl","XML","DBI","RODBC",
           "RPostgreSQL","rJava","xlsxjars","xlsx")

for (i in 1:length(pacotes))
{
  if (length(names(installed.packages()[,1])[names(installed.packages()[,1])==pacotes[i]])==0)
  {install.packages(pacotes[i], repos="http://cran.fiocruz.br/")}
  library(pacotes[i],character.only = TRUE)
}
rm(i,pacotes)

GENERICAverif <- function(nomes)
{
  #------ Desligando notacao cientifica
  options(scipen=999)
  
  #------ Organizando texto
  nomes.int <- rep(",",length(nomes)*2+1)
  nomes.int[seq(2,length(nomes)*2,2)] <- nomes
  nomes.int[c(1,length(nomes.int))] <- rep("",2)
  
  # CARREGANDO METADADOS ----------------------------------------
  
  #------ Abrindo conexao
  con <-  RODBC::odbcConnect("ipeadata",uid="",pwd="")
  
  #------ Consulta SQL 
  serie <- RODBC::sqlQuery(con,
                           paste0("SELECT ipea.vw_Valor.SERCODIGO, ", 
                                  "CAST (ipea.vw_Valor.VALDATA as NUMERIC) as VALDATA, ",
                                  "ipea.vw_Valor.VALVALOR FROM ipea.vw_Valor ",
                                  "WHERE ipea.vw_Valor.SERCODIGO IN (",
                                  paste(nomes.int, collapse = "'"),") ", 
                                  "and ipea.vw_Valor.VALVALOR IS NOT NULL order by VALDATA;"))
  
  #------ Fechando conexao
  RODBC::odbcClose(con)
  
  # ORGANIZANDO ----------------------------------------
  
  #------ Planilha Generica
  GENERICAv <- data.frame(VALDATA = unique(serie$VALDATA))
  for (i in 1:length(nomes))
  {
    GENERICAv <- merge(x = GENERICAv,
                       y = subset(x = serie,subset = serie$SERCODIGO == nomes[i],
                                  select = c("VALDATA","VALVALOR")),
                       by = "VALDATA",
                       all = TRUE)
    names(GENERICAv)[i+1] <- nomes[i]
  }
  
  #------ Editando formato de data
  GENERICAv$VALDATA <- as.Date(GENERICAv$VALDATA, origin = "1900-01-01")
  
  #------ Removendo possivel linha de NA
  GENERICAind <- data.frame(ind = is.na(GENERICAv[,-1]))
  erros <- sum(rowSums(GENERICAind)==ncol(GENERICAind))
  if(erros>0){GENERICAv <- GENERICAv[-which(rowSums(GENERICAind)==ncol(GENERICAind)),]}
  
  #------ Resultado
  return(GENERICAv)
}

SECEX12FOB12 <- function(gerarGen = TRUE, completa = FALSE)
{
  # LENDOS DADOS --------------------------------------
  
  #------ Texto informativo
  message("Requisitando dados via PostgreSQL")
  
  #------ Codigo dos paises (DB interno)
  codpaises <- codpaisesSECEX12FOB12$cod_paises
  
  #------ Abrindo conexao
  conAccess <- read.csv2("//Srjn3/area_corporativa/Projeto_IPEADATA/Geral/PacoteIpeadataRio/conPostgreSQL.csv")
  
  con <- DBI::dbConnect(drv = as.character(conAccess$drv), 
                        dbname = as.character(conAccess$dbname), 
                        host = as.character(conAccess$host),
                        port = conAccess$port,
                        user = as.character(conAccess$user),
                        password = as.character(conAccess$password))
  
  #------ Requerendo dados pelo SQL
  paises_exp <- DBI::dbGetQuery(con,
                                paste0("SELECT num_ano,num_mes,
                                       SUM(num_valor_fob) AS valor, cod_pais  
                                       FROM public.vw_comercio_exterior2
                                       WHERE imp_exp='e' AND num_ano>=",
                                       ifelse(completa,1990,(as.POSIXlt(Sys.Date()))$year+1900-10),
                                       "AND cod_pais IN (",
                                       paste0(codpaises, 
                                              collapse = ", "),") 
                                       GROUP BY num_ano,num_mes,
                                       cod_pais,nme_pais;"))
  
  paises_imp <- DBI::dbGetQuery(con,
                                paste0("SELECT num_ano,num_mes,
                                       SUM(num_valor_fob) AS valor, cod_pais  
                                       FROM public.vw_comercio_exterior2
                                       WHERE imp_exp='i' AND num_ano>=",
                                       ifelse(completa,1990,(as.POSIXlt(Sys.Date()))$year+1900-10),
                                       "AND cod_pais IN (",
                                       paste0(codpaises, 
                                              collapse = ", "),") 
                                       GROUP BY num_ano,num_mes,
                                       cod_pais,nme_pais;"))
  
  #------ Fechando conexao
  DBI::dbDisconnect(conn = con)
  
  # ORGANIZANDO --------------------------------------
  
  #------ Texto informativo
  message("Configurando planilha de atualizacao")
  
  exportacao <- data.frame(SERCODIGO = paste0("SECEX12_X",ifelse(nchar(as.character(paises_exp$cod_pais))==1,
                                                                 paste0("00",paises_exp$cod_pais),
                                                                 ifelse(nchar(as.character(paises_exp$cod_pais))==2,
                                                                        paste0("0",paises_exp$cod_pais),
                                                                        paises_exp$cod_pais)),
                                              "FOB12"),
                           VALDATA = as.Date(paste0(paises_exp$num_ano,"-",paises_exp$num_mes,"-15")),
                           VALVALOR = paises_exp$valor)
  exportacao <- exportacao[order(exportacao$SERCODIGO,exportacao$VALDATA),]
  
  importacao <- data.frame(SERCODIGO = paste0("SECEX12_M",ifelse(nchar(as.character(paises_imp$cod_pais))==1,
                                                                 paste0("00",paises_imp$cod_pais),
                                                                 ifelse(nchar(as.character(paises_imp$cod_pais))==2,
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
                          by = "VALDATA",all = TRUE)
    names(GENERICA_EXP)[i+1] <- as.character(unique(exportacao$SERCODIGO))[i]
  }
  
  GENERICA_IMP <- data.frame(VALDATA = sort(unique(importacao$VALDATA)))
  for (i in 1:length(unique(importacao$SERCODIGO)))
  {
    GENERICA_IMP <- merge(GENERICA_IMP,
                          subset(importacao,
                                 importacao$SERCODIGO == as.character(unique(importacao$SERCODIGO))[i])[,-1],
                          by = "VALDATA",all = TRUE)
    names(GENERICA_IMP)[i+1] <- as.character(unique(importacao$SERCODIGO))[i]
  }
  
  #------ Zerando os NAs (Faz sentido no caso de importacao e exportacao)
  for (i in 2:ncol(GENERICA_EXP)){GENERICA_EXP[,i] <- ifelse(is.na(GENERICA_EXP[,i]),0,GENERICA_EXP[,i])}
  for (i in 2:ncol(GENERICA_IMP)){GENERICA_IMP[,i] <- ifelse(is.na(GENERICA_IMP[,i]),0,GENERICA_IMP[,i])}

  #------ Eliminando objetos 
  rm(codpaises,conAccess,con,i,paises_exp,paises_imp,exportacao,importacao)
  
  #------ Comparando valores
  VALORES.BASE_EXP <- GENERICAverif(nomes = names(GENERICA_EXP)[-1])
  VALORES.BASE_IMP <- GENERICAverif(nomes = names(GENERICA_IMP)[-1])
  
  #------ Organizando data
  VALORES.BASE_EXP$VALDATA <- VALORES.BASE_EXP$VALDATA + 14
  VALORES.BASE_IMP$VALDATA <- VALORES.BASE_IMP$VALDATA + 14
  
  #------ Base auxiliar
  VALORES.BASE2_EXP <- merge(x = VALORES.BASE_EXP,y = GENERICA_EXP,by = "VALDATA")[,1:ncol(GENERICA_EXP)]
  VALORES.BASE2_IMP <- merge(x = VALORES.BASE_IMP,y = GENERICA_IMP,by = "VALDATA")[,1:ncol(GENERICA_IMP)]
  
  VALORES.BASE3_EXP <- merge(x = GENERICA_EXP,y = VALORES.BASE_EXP,by = "VALDATA")[,1:ncol(GENERICA_EXP)]
  VALORES.BASE3_IMP <- merge(x = GENERICA_IMP,y = VALORES.BASE_IMP,by = "VALDATA")[,1:ncol(GENERICA_IMP)]
  
  #------ Atualizar?
  atualizar <- FALSE
  if((nrow(GENERICA_EXP)>nrow(VALORES.BASE2_EXP))|
     (nrow(GENERICA_IMP)>nrow(VALORES.BASE2_IMP))){atualizar <- TRUE}
  if((nrow(GENERICA_EXP)==nrow(VALORES.BASE2_EXP)))
  {
    if(sum(VALORES.BASE2_EXP[,-1]!=GENERICA_EXP[,-1],na.rm = TRUE)>0){atualizar <- TRUE}
  }
  if((nrow(GENERICA_IMP)==nrow(VALORES.BASE2_IMP)))
  {
    if(sum(VALORES.BASE2_IMP[,-1]!=GENERICA_IMP[,-1],na.rm = TRUE)>0){atualizar <- TRUE}
  }
  
  if(gerarGen & atualizar)
  {
    # SALVANDO GENERICA --------------------------------------
    
    #------ Texto informativo
    message("Exportando planilha de atualizacao para //Srjn3/area_corporativa/Projeto_IPEADATA/ETL/Generica")
    
    #------ Exportando xls
    xlsx::write.xlsx(x = GENERICA_EXP,
                     file = "//Srjn3/area_corporativa/Projeto_IPEADATA/ETL/Generica/GENERICA_SECEX12exp.xls",
                     sheetName="Generica", row.names=FALSE, showNA=FALSE)
    
    xlsx::write.xlsx(x = GENERICA_IMP,
                     file = "//Srjn3/area_corporativa/Projeto_IPEADATA/ETL/Generica/GENERICA_SECEX12imp.xls",
                     sheetName="Generica", row.names=FALSE, showNA=FALSE)
    
    # ATUALIZANDO AUTOLOG --------------------------------------
    
    #------ Lendo autolog
    autolog <- read.csv2(file = "//Srjn3/area_corporativa/Projeto_IPEADATA/Geral/PacoteIpeadataRio/autolog.csv")
    
    #------ Editando estrutura
    autolog$data.hora <- as.character(autolog$data.hora)
    autolog$usuario <- as.character(autolog$usuario)
    autolog$acao <- as.character(autolog$acao)
    
    #------ Atualizando com credenciais
    r <- nrow(autolog) + 1
    autolog[r,] <- c(as.character(Sys.time()),Sys.getenv("USERNAME"),"GENERICA_SECEX12")
    
    #------ Ordenando
    autolog <- autolog[order(x = autolog$data.hora,decreasing = TRUE),]
    
    #------ Salvando autolog
    write.csv2(x = autolog,
               file = "//Srjn3/area_corporativa/Projeto_IPEADATA/Geral/PacoteIpeadataRio/autolog.csv",
               row.names = FALSE)
    
    #------ Eliminando objetos 
    rm(autolog,r)
  }
  
  # TEXTO RESUMO ---------------------------------------- 
  
  cat("\n")
  cat(paste("Relatorio do banco SECEX12 em",Sys.Date(),"\n"))
  cat("RESUMO \n")
  cat(paste("Numero de revisoes ....................",
            sum(VALORES.BASE2_EXP[,-1]!=VALORES.BASE3_EXP[,-1],na.rm = TRUE) +
            sum(VALORES.BASE2_IMP[,-1]!=VALORES.BASE3_IMP[,-1],na.rm = TRUE)),"\n")
  cat("\n")
  
  #------ Resultado
  return(list(GENERICA_EXPORTACAO = GENERICA_EXP,
              GENERICA_IMPORTACAO = GENERICA_IMP))
  
}

# --------------------------------------------------------- #
# AREA DE TESTE ---------------------------------------- 
a <- Sys.time()
A <- SECEX12FOB12(completa = TRUE)
Sys.time()-a