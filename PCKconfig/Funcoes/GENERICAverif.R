# --------------------------------------------------------- #
# |   INSTITUTO DE PESQUISA ECONOMICA APLICADA - IPEA     | #
# |                    PROJETO IPEADATA                   | #
# --------------------------------------------------------- #
# |   COORDENADOR: ERIVELTON P. GUEDES                    | #
# --------------------------------------------------------- #
# |   PROGRAMADOR: LUIZ EDUARDO S. GOMES                  | #
# --------------------------------------------------------- #
# |   GENERICA PARA VERIFICACAO                           | #
# --------------------------------------------------------- #

# CARREGANDO PACOTES ----------------------------------------

pacotes<-c("RODBC")

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

# --------------------------------------------------------- #
# AREA DE TESTE ---------------------------------------- 
nomes <- c("MTE12_SALMIN12","ABATE12_ABPENO12")
A <- GENERICAverif(nomes = nomes)
