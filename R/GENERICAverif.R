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

#' @title Planilha GENERICA para verificacao
#'
#' @description Retorna a planilha \code{GENERICA} para verificacao a
#' partir do \code{SERCODIGOTROLL}.
#'
#' @param nomes Vetor contendo o \code{SERCODIGOTROLL} da(s) serie(s) requisitada(s).
#'
#' @author Luiz Eduardo Gomes, \email{luiz.gomes@@ipea.gov.br} ou \email{gomes.leduardo@@gmail.com}.
#'
#' @examples
#' #------ Multiplas series, exibindo grafico
#' gen_verif <- GENERICAverif(nomes = c("gm12_DOW12","ABATE12_ABPENO12","MTE12_SALMIN12"))
#'
#' @export

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
