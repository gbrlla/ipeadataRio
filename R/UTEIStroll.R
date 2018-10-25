# --------------------------------------------------------- #
# |   INSTITUTO DE PESQUISA ECONOMICA APLICADA - IPEA     | #
# |                    PROJETO IPEADATA                   | #
# --------------------------------------------------------- #
# |   COORDENADOR: ERIVELTON P. GUEDES                    | #
# --------------------------------------------------------- #
# |   PROGRAMADOR: LUIZ EDUARDO S. GOMES                  | #
# --------------------------------------------------------- #
# |   FUNCOES UTEIS DO TROLL E MAIS                       | #
# --------------------------------------------------------- #

# --------------------------------------------------------- #
# DESCRIÇÃO BREVE:
# - Esta rotina retorna metadados das series.
# - A busca pode ser feita pelo codigo, periodicidade ou status.
# - Quando nenhuma serie e encotrada, retorna erro.
# --------------------------------------------------------- #

#' @title Encontra series
#'
#' @description Encontra series a partir do \code{SERCODIGOTROLL},
#' \emph{banco}, \emph{periodicidade} e/ou \emph{status}.
#'
#' @param serie Vetor contendo o \code{SERCODIGOTROLL} da(s) serie(s) requisitada(s).
#'
#' @param plotar Logico. Se \code{plotar = TRUE}, o grafico da(s) serie(s) e(sao) exibidos.
#' O \emph{default} e \code{TRUE}.
#'
#' @author Luiz Eduardo Gomes, \email{luiz.gomes@@ipea.gov.br} ou \email{gomes.leduardo@@gmail.com}.
#'
#' @note Foi limitado a exibicao de ate 5 (cinco) series simultaneamente.
#'
#' @examples
#' # ------ Serie unica, exibindo grafico
#' serie1 <- encontraSerie(serie = ("SGS366_CDI"))
#'
#' # ------ Multiplas series, exibindo grafico
#' serie2 <- encontraSerie(serie = c("ABATE12_ABQUBO12","ABATE12_ABQUBV12"))
#'
#' # ------ Multiplas series, nao exibindo grafico
#' serie3 <- encontraSerie(serie = c("gm12","ABATE12_ABPENO12","MTE12_SALMIN12"),
#'                          plotar = FALSE)
#'
#' # ------ Serie nao existente (retorna erro)
#' erro <- encontraSerie(serie = c("serie que n existe"))
#'
#' @export

encontraSerie <- function(serie, plotar = TRUE)
{
  # ------ Desligando notacao cientifica
  options(scipen=999)

  # CARREGANDO METADADOS ----------------------------------------

  # ------ Abrindo conexao
  con <-  RODBC::odbcConnect("ipeadata",uid="",pwd="")

  # ------ Consulta SQL
  metadados <- RODBC::sqlQuery(con,
                               paste0("SELECT dbo.SERIES.SERCODIGOTROLL, dbo.SERIES.PERID, ",
                                      "dbo.SERIES.SERSTATUS, dbo.SERIES.SERTIPO FROM dbo.SERIES ",
                                      "WHERE (((dbo.SERIES.SERTIPO)='N'));"))

  # ------ Fechando conexao
  RODBC::odbcClose(con)

  # ORGANIZANDO ARGUMENTOS ----------------------------------------

  # ------ Organizando texto - Removendo duplicatas, acentos e colocando em maiusculo
  serie <- unique(toupper(iconv(serie,to="ASCII//TRANSLIT")))

  # ------ Alterando a estrutura para character
  for (j in 1:dim(metadados)[2]){metadados[,j] <- as.character(metadados[,j])}
  rm(j)

  # ------ Alterando label -1/1/3/6/12 para DIARIA/MENSAL/TRIMESTRAL/SEMESTRAL/ANUAL
  metadados$PERID <- factor(metadados$PERID,
                            levels = c("-1","1","3","6","12"),
                            labels = c("DIARIA","MENSAL","TRIMESTRAL","SEMESTRAL","ANUAL"))

  # ------ Alterando label A/I para ATIVA/INATIVA
  metadados$SERSTATUS <- factor(metadados$SERSTATUS,
                                levels = c("A","I"),
                                labels = c("ATIVA","INATIVA"))

  # ENCONTRANDO AS SERIES ----------------------------------------

  # ------ Iniciando input
  ii <- NULL
  for (j in 1:length(serie))
  {
    # ------ Encontrando padroes no metadados - SERCODIGOTROLL (SERIES)
    if(length(i <- grep(paste0(serie[j]), metadados$SERCODIGOTROLL))) {ii <- c(ii,i)}

    # ------ Encontrando padroes no metadados - SERCODIGOTROLL (BANCOS)
    if(length(i <- grep(paste0(serie[j],"_"), metadados$SERCODIGOTROLL))) {ii <- c(ii,i)}

    # ------ Encontrando padroes no metadados - PERID
    if(length(i <- grep(serie[j], metadados$PERID))) {ii <- c(ii,i)}

    # ------ Encontrando padroes no metadados - SERSTATUS
    if(length(i <- grep(serie[j], metadados$SERSTATUS))) {ii <- c(ii,i)}
  }

  if(length(ii)>0){serinput <- metadados[unique(ii),]} else {
  stop("A(s) serie(s) nao existe(m) ou esta(o) com nome(s) incorreto(s)")}
  rm(i,ii,j)

  # ------ Ordem alfabetica
  serinput <- serinput[order(serinput$SERCODIGOTROLL),]

  # ------ Arrumando nome das linhas
  row.names(serinput) <- 1:dim(serinput)[1]

  # PLOTANDO GRAFICO ----------------------------------------

  if(plotar & nrow(serinput) > 5)
  {
    warning("Somente 5 series podem ser plotadas simultaneamente")
  }

  if(plotar & length(unique(serinput$PERID)) > 1)
  {
    warning("As series devem possuir mesma periodicidade")
  }

  if(plotar & nrow(serinput) <= 5 & length(unique(serinput$PERID))==1)
  {
    # ------ Requisitando valores
    generica.aux <- genericaVerif(serie = serinput$SERCODIGOTROLL)

    # ------ Condições para o banco de dados
    aux.data <- NA
    if(unique(serinput$PERID)=="DIARIA"){aux.data <- "day"}
    if(unique(serinput$PERID)=="MENSAL"){aux.data <- "month"}
    if(unique(serinput$PERID)=="TRIMESTRAL"){aux.data <- "3 months"}
    if(unique(serinput$PERID)=="SEMESTRAL"){aux.data <- "6 months"}
    if(unique(serinput$PERID)=="ANUAL"){aux.data <- "year"}

    # ------ Armazenamento de data e valores
    datas <- data.frame(VALDATA = seq(generica.aux$VALDATA[1],
                                      generica.aux$VALDATA[nrow(generica.aux)],
                                      by = aux.data))

    # ------ Juntando as datas com os valores
    aux <- merge(datas,generica.aux,by="VALDATA",all = T)

    # ------ Serie Temporal auxiliar
    ts.aux <- xts::xts(x = aux[,2:ncol(aux)],
                       order.by = as.Date(aux[,1], format='%Y-%m-%d'),
                       names = names(aux)[-1])

    # ------ Grafico dinamico
    if(ncol(aux)==2)
    {
      print(dygraphs::dySeries(dygraph = dygraphs::dyRangeSelector(dygraphs::dygraph(data = ts.aux)),name = "V1",label = names(aux)[-1]))
    } else {print(dygraphs::dySeries(dygraph = dygraphs::dyRangeSelector(dygraphs::dygraph(data = ts.aux))))}
  }

  # RESULTADO ----------------------------------------

  # ------ Resultado

  return(serinput)
}

#' @title Registro dos responsaveis das series do IpeaData (Macro)
#'
#' @description Conjunto de dados contendo informacoes dos responsaveis das
#' series do IpeaData (Macro) para registro interno.
#'
#' @format Banco de dados com 11042 observacao(oes) e 2 variavel(is):
#' \describe{
#'   \item{Variavel}{Codigo}
#'   \item{Responsavel}{Nome do responsavel}
#' }

"series.automaticas"

#' @title Registro da interface de atualizacao das series
#'
#' @description Registro da interface de atualizacao das series: Generica ou SGS.
#'
#' @format Banco de dados com 13783 observacao(oes) e 6 variavel(is):
#' \describe{
#'   \item{SERCODIGO}{Codigo}
#'   \item{PERNOME}{Periodicidade}
#'   \item{SERSTATUS}{Status}
#'   \item{interface}{Interface}
#'   \item{CodSGS}{Codigo SGS}
#'   \item{BASNOME}{Nome da base}
#' }

"lista.interface"

# --------------------------------------------------------- #
# DESCRIÇÃO BREVE:
# - Esta rotina retorna o atraso das series.
# - A busca pode ser feita pelo codigo, periodicidade ou status.
# - Quando nenhuma serie e encotrada, retorna erro.
# --------------------------------------------------------- #

#' @title Situacao das series
#'
#' @description Retorna a situacao das series com relacao a atraso a partir do \code{SERCODIGOTROLL},
#' \emph{banco}, \emph{periodicidade} e/ou \emph{status}.
#'
#' @param serie Vetor contendo o \code{SERCODIGOTROLL},\emph{banco}, \emph{periodicidade}
#'  e/ou \emph{status} da(s) serie(s) requisitada(s).
#'
#' @param exportar Logico. Se \code{exportar = TRUE}, um relatorio \code{.xls} e exportado para o
#' diretorio do \code{situavar}. O \emph{default} e \code{TRUE}.
#'
#' @details
#' Para series \emph{DIARIAS}, a data de referencia sera dada pelo dia corrente.
#'
#' Para series \emph{MENSAIS}, \emph{TRIMESTRAIS} E \emph{SEMESTRAIS},
#' a data de referencia sera dada por AAAA-MM-01.
#'
#' Para series \emph{ANUAIS} em diante, a data de referencia sera dada por AAAA-01-01.
#'
#' Assim, o \emph{default} para series \emph{DIARIAS} e estarem atualizadas no mesmo dia e
#' para as restantes, e estarem atualizadas na unidade de tempo anterior.
#'
#' Calculo:
#' Se \bold{(Data de referencia - Defasagem - Data final > 0)} entao a serie esta atrasada.
#'
#' @author Luiz Eduardo Gomes, \email{luiz.gomes@@ipea.gov.br} ou \email{gomes.leduardo@@gmail.com}.
#'
#' @note A nomenclatura dada ao arquivo sera \emph{situavarAAAAMMDDhhmmss.xls}.
#' Quando \code{serie} e unitario, o arquivo e nomeado da forma padrao com a
#' adicao do conteudo de \code{serie}.
#'
#' @examples
#' # ------ Multiplos Bancos e serie
#' sit1 <- situavar(serie = c("PIMPFN12","GM366","ANDIMA4","CONFAZ12_ICMSSP12"))
#'
#' # ------ Periodicidade e bancos
#' sit2 <- situavar(serie = c("diaria","IGP12"))
#'
#' # ------ Serie nao existente (retorna erro)
#' erro <- situavar(serie = c("serie que n existe"))
#'
#' @export

situavar <- function(serie, exportar = TRUE)
{
  # ------ Organizando texto - Removendo duplicatas, acentos e colocando em maiusculo
  serie <- unique(toupper(iconv(serie,to="ASCII//TRANSLIT")))

  # ------ Encontrando a serie
  serinput <- encontraSerie(serie = serie,plotar = FALSE)

  # ------ Desligando notacao cientifica
  options(scipen=999)

  # CARREGANDO METADADOS ----------------------------------------

  # ------ Abrindo conexao
  con <-  RODBC::odbcConnect("ipeadata",uid="",pwd="")

  # ------ Solicitando metadados das series
  metadados <- data.frame(RODBC::sqlQuery(con,
                                          (paste0("SELECT dbo.SERIES.SERCODIGOTROLL, ",
                                                  "CAST (dbo.SERIES.SERMINDATA as NUMERIC) as SERMINDATA, ",
                                                  "CAST (dbo.SERIES.SERMAXDATA as NUMERIC) as SERMAXDATA, ",
                                                  "dbo.SERIES.PERID, dbo.SERIES.SERSTATUS, ",
                                                  "dbo.SERIES.SERPRAZOATUALIZACAO, ",
                                                  "dbo.SERIES.SERRESPONSAVEL FROM dbo.SERIES ",
                                                  "WHERE (((dbo.SERIES.SERCODIGOTROLL) IN (",
                                                  paste0("'",serinput$SERCODIGOTROLL,"'", collapse = ", "),")));"))),
                          PERID2 = serinput$PERID,
                          STATUS_ATRASO = NA)

  # ------ Fechando conexao
  RODBC::odbcClose(con)

  # ORGANIZANDO ARGUMENTOS ----------------------------------------

  # ------ Tornando datas padroes
  metadados$SERMINDATA <- as.Date(metadados$SERMINDATA, origin = "1900-01-01")
  metadados$SERMAXDATA <- as.Date(metadados$SERMAXDATA, origin = "1900-01-01")

  # CALCULANDO OS ATRASOS ----------------------------------------

  # --------------------------------------------------------- #
  # IMPORTANTE:
  # - Para series DIARIAS, a data de referencia sera dada pelo dia corrente.
  # - Para series MENSAIS, TRIMESTRAIS E SEMESTRAIS, a data de referencia sera dada por AAAA-MM-01.
  # - Para series ANUAIS em diante, a data de referencia sera dada por AAAA-01-01.
  # --------------------------------------------------------- #

  data.ref <- as.Date(paste0((as.POSIXlt(Sys.Date())$year)+1900,"-",
                             ifelse(metadados$PERID[1]>=12,"01",as.POSIXlt(Sys.Date())$mon+1),"-",
                             ifelse(metadados$PERID[1]!=-1,"01",as.POSIXlt(Sys.Date())$mday)))

  # --------------------------------------------------------- #
  # OBSERVACAO:
  # - o padrao para series DIARIAS e estar atualizada no mesmo dia.
  # - o padrao para as series restante e estar atualizada na unidade de tempo anterior.
  # CALCULO:
  # - Se (Data de referencia - Defasagem - Data final > 0) entao
  #   SERIE ATRASADA!
  # - Senao, OK.
  # --------------------------------------------------------- #

  # ------ Calculando
  metadados$STATUS_ATRASO <- ifelse(metadados$SERSTATUS=="A",
                                    ifelse(as.numeric(data.ref-metadados$SERPRAZOATUALIZACAO-metadados$SERMAXDATA)>0,
                                           as.numeric(Sys.Date()-metadados$SERPRAZOATUALIZACAO-metadados$SERMAXDATA)-
                                             2*(30*ifelse(metadados$PERID==-1,0,metadados$PERID)),
                                           0),
                                    -.5)

  # ------ Desfazendo o erro de defasagem
  metadados$STATUS_ATRASO <- ifelse(metadados$STATUS_ATRASO<(-.5),0,metadados$STATUS_ATRASO)

  # ------ Inputando erro de data maior
  metadados$STATUS_ATRASO <- ifelse(metadados$SERMAXDATA>Sys.Date(),999999999,metadados$STATUS_ATRASO)

  # RESULTADO ----------------------------------------

  # ------ Adicionando interface
  metadados <- merge(x = metadados,
                     y = data.frame(SERCODIGOTROLL = ipeadataRio::lista.interface$SERCODIGO,
                                    INTERFACE = ipeadataRio::lista.interface$interface),
                     by = "SERCODIGOTROLL")

  # ------ Ordenando por n de atrasos
  metadados <- metadados[order(metadados$STATUS_ATRASO,decreasing = T),]

  # ------ Banco resultado
  saida <- data.frame(Variavel = metadados$SERCODIGOTROLL,
                      Data_Inicial = metadados$SERMINDATA,
                      Data_Final = metadados$SERMAXDATA,
                      Defasagem_Dias = metadados$SERPRAZOATUALIZACAO,
                      Situacao = ifelse(metadados$STATUS_ATRASO==0,
                                        "Variavel atualizada - OK",
                                        ifelse(metadados$STATUS_ATRASO>0 & metadados$STATUS_ATRASO<999999999,
                                               paste("Variavel desatualizada",metadados$STATUS_ATRASO,"dia(s)   <=="),
                                               ifelse(metadados$STATUS_ATRASO==999999999,
                                                      "Erro de data (!!)",
                                                      "Serie Inativa"))),
                      Periodicidade = paste0(substr(x = metadados$PERID2,start = 1,stop = 1),
                                      tolower(substr(x = metadados$PERID2,start = 2,stop = 99))),
                      Responsavel = metadados$SERRESPONSAVEL,
                      Interface = metadados$interface)

  # ------ Exportar?
  if(exportar)
  {
    # ------ Salvando relatorio
    xlsx::write.xlsx(x = saida,
                     file = file.path("","","Srjn3","area_corporativa","Projeto_IPEADATA","Geral",
                                      "PacoteIpeadataRio","situavar",
                                      paste0("situavar",
                                      substr(Sys.time(),1,4),substr(Sys.time(),6,7),
                                      substr(Sys.time(),9,10),substr(Sys.time(),12,13),
                                      substr(Sys.time(),15,16),substr(Sys.time(),18,19),
                                      ifelse(length(serie)==1,paste0("_",serie),""),".xls")),
                     sheetName="Generica", row.names=FALSE, showNA=FALSE)
  }

  # TEXTO RESUMO ----------------------------------------

  cat("\n")
  if(length(serie)==1)
  {
    cat(paste("Relatorio das variaveis do arquivo:",serie,"em",Sys.Date(),"\n"))
  } else {
    cat(paste("Relatorio das variaveis do arquivo em",Sys.Date(),"\n"))
  }
  if(length(unique(serinput$PERID))==1){cat(paste("Periodicidade:",serinput$PERID[1],"\n"))}
  cat("\n")
  cat("RESUMO \n")
  cat("Numero de Variaveis \n")
  cat(paste("Total .................................",nrow(metadados)),"\n")
  cat(paste("Atualizadas ...........................",sum(metadados$STATUS_ATRASO==0,na.rm = T)),"\n")
  cat(paste("Desatualizadas ........................",max(0,sum(metadados$STATUS_ATRASO>0,na.rm = T)-sum(metadados$SERMAXDATA>Sys.Date(),na.rm = T))),"\n")
  cat(paste("Inativas ..............................",sum(metadados$STATUS_ATRASO==-.5,na.rm = T)),"\n")
  cat(paste("Erro ou Data maior que", Sys.Date(), ".....",sum(metadados$SERMAXDATA>Sys.Date(),na.rm = T)),"\n")
  cat(paste("Metadados nao preenchidos  ............",sum(is.na(metadados$STATUS_ATRASO))),"\n")
  cat("\n")

  # ------ Resultado
  return(saida)
}

# --------------------------------------------------------- #
# DESCRIÇÃO BREVE:
# - Esta rotina retorna total de dados faltantes em uma serie.
# - A busca pode ser feita pelo codigo, periodicidade ou status.
# - Quando nenhuma serie e encotrada, retorna erro.
# --------------------------------------------------------- #

#' @title Descontinuidade
#'
#' @description Retorna a descontinuidade existente nas series a partir do \code{SERCODIGOTROLL},
#' \emph{banco}, \emph{periodicidade} e/ou \emph{status}.
#'
#' @param serie Vetor contendo o \code{SERCODIGOTROLL},\emph{banco}, \emph{periodicidade}
#'  e/ou \emph{status} da(s) serie(s) requisitada(s).
#'
#' @param plotar Logico. Se \code{plotar = TRUE}, o grafico da(s) serie(s) e(sao) exibidos.
#' O \emph{default} e \code{TRUE}.
#'
#' @author Luiz Eduardo Gomes, \email{luiz.gomes@@ipea.gov.br} ou \email{gomes.leduardo@@gmail.com}.
#'
#' @note Foi limitado a exibicao de ate 5 (cinco) series simultaneamente.
#'
#' @examples
#' # ------ Multiplas series, exibindo grafico
#' desc1 <- dadosFaltantes(serie = c("gm12_DOW12","ABATE12_ABPENO12","MTE12_SALMIN12"))
#'
#' # ------ Banco, nao exibindo grafico
#' desc2 <- dadosFaltantes(serie = c("TRIMESTRAL"),plotar = FALSE) # pode demandar tempo!
#'
#' # ------ Plotando mais de 5 series, retorna erro
#' erro <- dadosFaltantes(serie = c("TRIMESTRAL")) # pode demandar tempo!
#'
#' @export

dadosFaltantes <- function(serie, plotar = TRUE)
{
  # ------ Organizando texto - Removendo duplicatas, acentos e colocando em maiusculo
  serie <- unique(toupper(iconv(serie,to="ASCII//TRANSLIT")))

  # ------ Encontrando a serie
  serinput <- encontraSerie(serie = serie,plotar = plotar)

  # ------ Retornando erro se possui series diarias
  if(sum(serinput$PERID=="DIARIA")>0) stop("Series com periodicidade DIARIA nao sao aplicaveis")

  # ------ Desligando notacao cientifica
  options(scipen=999)

  # ------ Banco auxiliar
  saida <- data.frame(serinput$SERCODIGOTROLL, serinput$SERSTATUS, N_MISS = NA)

  # ------ Atualizacao da barra de progresso
  update.step <- ifelse(nrow(saida)>5,max(5, floor(nrow(saida)/100)),0)

  # ------ Barra de progresso
  if(update.step>0){pb <- txtProgressBar(max = nrow(saida), style = 3)}

  # CARREGANDO VALORES ----------------------------------------

  for (i in 1:dim(serinput)[1])
  {
    # ------ Abrindo conexao
    con <-  RODBC::odbcConnect("ipeadata",uid="",pwd="")

    valores <- RODBC::sqlQuery(con,
                               (paste0("SELECT ipea.vw_Valor.SERCODIGO, ",
                                       "CAST (ipea.vw_Valor.VALDATA as NUMERIC) as VALDATA, ",
                                       "ipea.vw_Valor.VALVALOR FROM ipea.vw_Valor WHERE (((ipea.vw_Valor.SERCODIGO)='",
                                       serinput$SERCODIGOTROLL[i],"')) order by VALDATA;")))

    # ------ Fechando conexao
    RODBC::odbcClose(con)

    if (nrow(valores)>0)
    {
      # ------ Tornando datas padrões
      valores$VALDATA <- as.Date(valores$VALDATA, origin = "1900-01-01")

      # ------ Condições para o banco de dados
      if(serinput$PERID[i]=="MENSAL") aux.data <- "month"
      if(serinput$PERID[i]=="TRIMESTRAL") aux.data <- "3 months"
      if(serinput$PERID[i]=="SEMESTRAL") aux.data <- "6 months"
      if(serinput$PERID[i]=="ANUAL") aux.data <- "year"

      # ------ Armazenamento de data e valores
      datas <- data.frame(VALDATA = seq(valores$VALDATA[1],
                                        valores$VALDATA[nrow(valores)],
                                        by = aux.data))

      # ------ Juntando as datas com os valores
      valores <- merge(valores,datas,by="VALDATA",all = T)

      # ------ Contagem dados faltantes
      saida$N_MISS[i] <- sum(is.na(valores$VALVALOR))
    } else {saida$N_MISS[i] <- 999999999}

    # ------ Barra de progresso na tela
    if(update.step!=0 & (i%%update.step)==0){setTxtProgressBar(pb, i)}
  }

  # ------ Fechando conexao da barra de progresso
  if(update.step!=0){close(pb)}

  # RESULTADO ----------------------------------------

  # ------ Serie em ordem alfabetica com respectivo numero de faltantes (missing)
  saida <- saida[order(saida$N_MISS,decreasing = T),]

  # ------ Banco resultado
  saida2 <- data.frame(Variavel = saida$serinput.SERCODIGOTROLL,
                       Status = saida$serinput.SERSTATUS,
                       Dados_Faltantes = ifelse(saida$N_MISS==0,
                                                "Variavel completa - OK",
                                                ifelse(saida$N_MISS>0 & saida$N_MISS<999999999,
                                                       paste("Variavel com",saida$N_MISS,"dado(s) faltante(s)   <=="),
                                                       "Variavel vazia   (!!)")))

  # TEXTO RESUMO ----------------------------------------

  cat("\n")
  if(length(serie)==1)
  {
    cat(paste("Relatorio das variaveis do arquivo:",serie,"em",Sys.Date(),"\n"))
  } else {
    cat(paste("Relatorio das variaveis do arquivo em",Sys.Date(),"\n"))
  }
  if(length(unique(serinput$PERID))==1){cat(paste("Periodicidade:",serinput$PERID[1],"\n"))}
  cat("\n")
  cat("RESUMO \n")
  cat("Numero de Variaveis \n")
  cat(paste("Total .................................",nrow(saida)),"\n")
  cat(paste("Completas .............................",sum(saida$N_MISS==0,na.rm = T)),"\n")
  cat(paste("Incompletas ...........................",sum(saida$N_MISS>0 & saida$N_MISS<999999999,na.rm = T)),"\n")
  cat(paste("Vazias ................................",sum(saida$N_MISS==999999999,na.rm = T)),"\n")
  cat("\n")

  # ------ Resultado
  return(saida2)
}

# --------------------------------------------------------- #
# DESCRIÇÃO BREVE:
# - Esta rotina retorna series com discordancia nas datas.
# - A busca pode ser feita pelo codigo, periodicidade ou status.
# - Quando nenhuma serie e encotrada, retorna aviso.
# --------------------------------------------------------- #

#' @title Discordancia de datas
#'
#' @description Retorna a discordancia de datas nas series a partir do \code{SERCODIGOTROLL},
#' \emph{banco}, \emph{periodicidade} e/ou \emph{status}.
#'
#' @param serie Vetor contendo o \code{SERCODIGOTROLL},\emph{banco}, \emph{periodicidade}
#'  e/ou \emph{status} da(s) serie(s) requisitada(s).
#'
#' @author Luiz Eduardo Gomes, \email{luiz.gomes@@ipea.gov.br} ou \email{gomes.leduardo@@gmail.com}.
#'
#' @examples
#' # ------ Multiplas series
#' disc <- discordDatas(serie = c("gm12_DOW12","ABATE12_ABPENO12","MTE12_SALMIN12"))
#'
#' @export

discordDatas <- function(serie)
{
  # ------ Abrindo conexao
  con <-  RODBC::odbcConnect("ipeadata",uid="",pwd="")

  # ------ Consulta SQL
  metadados <- RODBC::sqlQuery(con,
                               paste0("SELECT dbo.SERIES.SERCODIGOTROLL, dbo.SERIES.SERMAXDATA ",
                                      "FROM dbo.SERIES ",
                                      "WHERE (((dbo.SERIES.SERTIPO)='N'));"))

  # ------ Convertendo para texto evitando erros
  metadados$SERMAXDATA <- as.character(metadados$SERMAXDATA)

  # ------ Requerendo dados
  if(length(serie) < 300)
  {
    dados <- genericaVerif(serie = serie)
  } else {
    sec <- c(seq(0, length(serie), 100), length(serie))
    dados <- genericaVerif(serie = serie[(sec[1] + 1):sec[2]])
    for (l in 2:(length(sec) - 1))
    {
      dados <- merge(x = dados,
                     y = genericaVerif(serie = serie[(sec[l] + 1):sec[l + 1]]),
                     by = "VALDATA")
    }
  }

  # ------ Convertendo para texto evitando erros
  dados$VALDATA <- as.character(dados$VALDATA)

  # COMPARANDO ----------------------------------------
  aux <- data.frame(NULL)

  # ------ Buscando datas finais
  for (i in 2:ncol(dados))
  {
    aux[i-1,1] <- names(dados)[i]
    aux[i-1,2] <- ifelse(test = nrow(utils::tail(x = na.exclude(dados[,c(1,i)]), n = 1)[1]) > 0,
                         yes = utils::tail(x = na.exclude(dados[,c(1,i)]), n = 1)[1],
                         no = "")
  }

  # ------ Nomeando
  names(aux) <- c("SERCODIGOTROLL","VALDATA")

  # ------ Comparando com o do banco
  aux <- merge(x = aux, y = metadados, by = "SERCODIGOTROLL")
  aux[,4] <- ifelse(test = aux$VALDATA == as.Date(aux$SERMAXDATA), yes = 1, no = 0)

  # RESULTADO ----------------------------------------

  # ------ Banco resultado
  saida <- subset(x = aux,subset = aux[,4] == 0)

  # ------ Resultado
  if(nrow(saida) == 0){cat("Nao ha discordancia entre datas!")} else {
    return(saida[,-4])
  }
}
