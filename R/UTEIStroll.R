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

#' @export

encontra.serie <- function(serie, plotar = TRUE)
{
  #------ Desligando notacao cientifica
  options(scipen=999)
  
  # CARREGANDO METADADOS ----------------------------------------
  
  #------ Abrindo conexao
  con <-  RODBC::odbcConnect("ipeadata",uid="",pwd="")
  
  #------ Consulta SQL 
  metadados <- RODBC::sqlQuery(con,
                               paste0("SELECT dbo.SERIES.SERCODIGOTROLL, dbo.SERIES.PERID, ", 
                                      "dbo.SERIES.SERSTATUS, dbo.SERIES.SERTIPO FROM dbo.SERIES ",
                                      "WHERE (((dbo.SERIES.SERTIPO)='N'));"))
  
  #------ Fechando conexao
  RODBC::odbcClose(con)
  
  # ORGANIZANDO ARGUMENTOS ----------------------------------------

  #------ Organizando texto - Removendo duplicatas, acentos e colocando em maiusculo 
  serie <- unique(toupper(iconv(serie,to="ASCII//TRANSLIT")))
  
  #------ Alterando a estrutura para character 
  for (j in 1:dim(metadados)[2]){metadados[,j] <- as.character(metadados[,j])}
  rm(j)
  
  #------ Alterando label -1/1/3/6/12 para DIARIA/MENSAL/TRIMESTRAL/SEMESTRAL/ANUAL 
  metadados$PERID <- factor(metadados$PERID,
                            levels = c("-1","1","3","6","12"),
                            labels = c("DIARIA","MENSAL","TRIMESTRAL","SEMESTRAL","ANUAL"))
  
  #------ Alterando label A/I para ATIVA/INATIVA 
  metadados$SERSTATUS <- factor(metadados$SERSTATUS,
                                levels = c("A","I"),
                                labels = c("ATIVA","INATIVA"))
  
  # ENCONTRANDO AS SERIES ----------------------------------------
  
  #------ Iniciando input 
  ii <- NULL
  for (j in 1:length(serie))
  {
    #------ Encontrando padroes no metadados - SERCODIGOTROLL (SERIES) 
    if(length(i <- grep(paste0(serie[j]), metadados$SERCODIGOTROLL))) {ii <- c(ii,i)}
    
    #------ Encontrando padroes no metadados - SERCODIGOTROLL (BANCOS) 
    if(length(i <- grep(paste0(serie[j],"_"), metadados$SERCODIGOTROLL))) {ii <- c(ii,i)}
    
    #------ Encontrando padroes no metadados - PERID 
    if(length(i <- grep(serie[j], metadados$PERID))) {ii <- c(ii,i)}
    
    #------ Encontrando padroes no metadados - SERSTATUS 
    if(length(i <- grep(serie[j], metadados$SERSTATUS))) {ii <- c(ii,i)}
  }
  
  if(length(ii)>0){serinput <- metadados[unique(ii),]} else {
  stop("A(s) serie(s) nao existe(m) ou esta(o) com nome(s) incorreto(s)")}
  rm(i,ii,j)

  #------ Ordem alfabetica 
  serinput <- serinput[order(serinput$SERCODIGOTROLL),]
  
  #------ Arrumando nome das linhas 
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
    #------ Requisitando valores
    generica.aux <- GENERICAverif(nomes = serinput$SERCODIGOTROLL)
    
    #------ Condições para o banco de dados
    aux.data <- NA
    if(unique(serinput$PERID)=="DIARIA"){aux.data <- "day"}
    if(unique(serinput$PERID)=="MENSAL"){aux.data <- "month"}
    if(unique(serinput$PERID)=="TRIMESTRAL"){aux.data <- "3 months"}
    if(unique(serinput$PERID)=="SEMESTRAL"){aux.data <- "6 months"}
    if(unique(serinput$PERID)=="ANUAL"){aux.data <- "year"}
    
    #------ Armazenamento de data e valores  
    datas <- data.frame(VALDATA = seq(generica.aux$VALDATA[1],
                                      generica.aux$VALDATA[nrow(generica.aux)],
                                      by = aux.data))
    
    #------ Juntando as datas com os valores  
    aux <- merge(datas,generica.aux,by="VALDATA",all = T)
    
    #------ Serie Temporal auxiliar
    ts.aux <- xts::xts(x = aux[,2:ncol(aux)], 
                       order.by = as.Date(aux[,1], format='%Y-%m-%d'),
                       names = names(aux)[-1])
     
    #------ Grafico dinamico
    if(ncol(aux)==2)
    {
      print(dygraphs::dySeries(dygraph = dygraphs::dyRangeSelector(dygraphs::dygraph(data = ts.aux)),name = "V1",label = names(aux)[-1]))
    } else {print(dygraphs::dySeries(dygraph = dygraphs::dyRangeSelector(dygraphs::dygraph(data = ts.aux))))}
  }
  
  # RESULTADO ----------------------------------------
  
  #------ Resultado
  
  return(serinput)
}

# --------------------------------------------------------- #
# DESCRIÇÃO BREVE:
# - Esta rotina retorna o atraso das series.
# - A busca pode ser feita pelo codigo, periodicidade ou status.
# - Quando nenhuma serie e encotrada, retorna erro.
# --------------------------------------------------------- #

#' @export

situavar <- function(serie, exportar = TRUE, saida.aux = FALSE)
{
  #------ Organizando texto - Removendo duplicatas, acentos e colocando em maiusculo 
  serie <- unique(toupper(iconv(serie,to="ASCII//TRANSLIT")))
  
  #------ Encontrando a serie  
  serinput <- encontra.serie(serie = serie,plotar = FALSE)
  
  #------ Desligando notacao cientifica  
  options(scipen=999)
  
  # CARREGANDO METADADOS ----------------------------------------

  #------ Abrindo conexao
  con <-  RODBC::odbcConnect("ipeadata",uid="",pwd="")
  
  #------ Solicitando metadados das series 
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
  
  #------ Fechando conexao
  RODBC::odbcClose(con)
  
  # ORGANIZANDO ARGUMENTOS ---------------------------------------- 
  
  #------ Tornando datas padroes 
  metadados$SERMINDATA <- as.Date(metadados$SERMINDATA, origin = "1900-01-01")
  metadados$SERMAXDATA <- as.Date(metadados$SERMAXDATA, origin = "1900-01-01")

  # CALCULANDO OS ATRASOS ---------------------------------------- 
  
  # --------------------------------------------------------- #
  # IMPORTANTE:
  # - Para series DIARIAS, a data de referencia sera dada pelo dia corrente. 
  # - Para series MENSAIS, TRIMESTRAIS E SEMESTRAIS, a data de referencia sera dada por YYYY-MM-01.
  # - Para series ANUAIS em diante, a data de referencia sera dada por YYYY-01-01.
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
  
  #------ Calculando 
  metadados$STATUS_ATRASO <- ifelse(metadados$SERSTATUS=="A",
                                    ifelse(as.numeric(data.ref-metadados$SERPRAZOATUALIZACAO-metadados$SERMAXDATA)>0,
                                           as.numeric(Sys.Date()-metadados$SERPRAZOATUALIZACAO-metadados$SERMAXDATA)-
                                             2*(30*ifelse(metadados$PERID==-1,0,metadados$PERID)),
                                           0),
                                    -.5)
  
  #------ Desfazendo o erro de defasagem 
  metadados$STATUS_ATRASO <- ifelse(metadados$STATUS_ATRASO<(-.5),0,metadados$STATUS_ATRASO)
  
  #------ Inputando erro de data maior 
  metadados$STATUS_ATRASO <- ifelse(metadados$SERMAXDATA>Sys.Date(),999999999,metadados$STATUS_ATRASO)
  
  # RESULTADO ---------------------------------------- 
  
  #------ Ordenando por n de atrasos 
  metadados <- metadados[order(metadados$STATUS_ATRASO,decreasing = T),]
  
  #------ Banco resultado 
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
                      Responsavel = metadados$SERRESPONSAVEL)
  
  if(saida.aux)
  {
    #------ Banco auxiliar 
    saida <- data.frame(Variavel = metadados$SERCODIGOTROLL, 
                        Data_Inicial = metadados$SERMINDATA, 
                        Data_Final = metadados$SERMAXDATA,
                        Defasagem_Dias = metadados$SERPRAZOATUALIZACAO,
                        Situacao = ifelse(test = metadados$STATUS_ATRASO==0 | metadados$STATUS_ATRASO==999999999,
                                          yes = 0,no = metadados$STATUS_ATRASO),
                        Periodicidade = paste0(substr(x = metadados$PERID2,start = 1,stop = 1),
                                               tolower(substr(x = metadados$PERID2,start = 2,stop = 99))),
                        Responsavel = metadados$SERRESPONSAVEL)
    
    #------ Substituindo
    saida$Situacao <- ifelse(test = saida$Situacao < 0,
                             yes = 0,
                             no = saida$Situacao)
  }

  
  #------ Exportar?
  if(exportar & !saida.aux)
  {
    #------ Salvando relatorio
    xlsx::write.xlsx(x = saida,
                     file = file.path("","","Srjn3","area_corporativa","Projeto_IPEADATA","Geral","PacoteIpeadataRio","situavar","situavar",
                                      substr(Sys.time(),1,4),substr(Sys.time(),6,7),
                                      substr(Sys.time(),1,4),substr(Sys.time(),6,7),
                                      substr(Sys.time(),9,10),substr(Sys.time(),12,13),
                                      substr(Sys.time(),15,16),substr(Sys.time(),18,19),
                                      ifelse(length(serie)==1,paste0("_",serie),""),".xls"),
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
  cat(paste("Desatualizadas ........................",sum(metadados$STATUS_ATRASO>0,na.rm = T)),"\n")
  cat(paste("Inativas ..............................",sum(metadados$STATUS_ATRASO==-.5,na.rm = T)),"\n")
  cat(paste("Erro ou Data maior que", Sys.Date(), ".....",sum(metadados$SERMAXDATA>Sys.Date(),na.rm = T)),"\n")
  cat("\n")
  
  #------ Resultado
  return(saida)
}

# --------------------------------------------------------- #
# DESCRIÇÃO BREVE: 
# - Esta rotina retorna total de dados faltantes em uma serie.
# - A busca pode ser feita pelo codigo, periodicidade ou status.
# - Quando nenhuma serie e encotrada, retorna erro.
# --------------------------------------------------------- #

#' @export

dados.faltantes <- function(serie, plotar = TRUE)
{
  #------ Organizando texto - Removendo duplicatas, acentos e colocando em maiusculo 
  serie <- unique(toupper(iconv(serie,to="ASCII//TRANSLIT")))
  
  #------ Encontrando a serie  
  serinput <- encontra.serie(serie = serie,plotar = plotar)
  
  #------ Retornando erro se possui series diarias  
  if(sum(serinput$PERID=="DIARIA")>0) stop("Series com periodicidade DIARIA nao sao aplicaveis")
  
  #------ Desligando notacao cientifica  
  options(scipen=999)
  
  #------ Banco auxiliar
  saida <- data.frame(serinput$SERCODIGOTROLL, serinput$SERSTATUS, N_MISS = NA)
  
  #------ Atualizacao da barra de progresso 
  update.step <- ifelse(nrow(saida)>5,max(5, floor(nrow(saida)/100)),0)
  
  #------ Barra de progresso 
  if(update.step>0){pb <- txtProgressBar(max = nrow(saida), style = 3)}
  
  # CARREGANDO VALORES ---------------------------------------- 
  
  for (i in 1:dim(serinput)[1])
  {
    #------ Abrindo conexao
    con <-  RODBC::odbcConnect("ipeadata",uid="",pwd="")
    
    valores <- RODBC::sqlQuery(con,
                               (paste0("SELECT ipea.vw_Valor.SERCODIGO, ", 
                                       "CAST (ipea.vw_Valor.VALDATA as NUMERIC) as VALDATA, ",
                                       "ipea.vw_Valor.VALVALOR FROM ipea.vw_Valor WHERE (((ipea.vw_Valor.SERCODIGO)='",
                                       serinput$SERCODIGOTROLL[i],"')) order by VALDATA;")))
    
    #------ Fechando conexao
    RODBC::odbcClose(con)
    
    if (nrow(valores)>0)
    {
      #------ Tornando datas padrões 
      valores$VALDATA <- as.Date(valores$VALDATA, origin = "1900-01-01")
      
      #------ Condições para o banco de dados 
      if(serinput$PERID[i]=="MENSAL") aux.data <- "month"
      if(serinput$PERID[i]=="TRIMESTRAL") aux.data <- "3 months"
      if(serinput$PERID[i]=="SEMESTRAL") aux.data <- "6 months"
      if(serinput$PERID[i]=="ANUAL") aux.data <- "year"
      
      #------ Armazenamento de data e valores  
      datas <- data.frame(VALDATA = seq(valores$VALDATA[1],
                                        valores$VALDATA[nrow(valores)],
                                        by = aux.data))
      
      #------ Juntando as datas com os valores  
      valores <- merge(valores,datas,by="VALDATA",all = T)
      
      #------ Contagem dados faltantes 
      saida$N_MISS[i] <- sum(is.na(valores$VALVALOR))
    } else {saida$N_MISS[i] <- 999999999}
    
    #------ Barra de progresso na tela 
    if(update.step!=0 & (i%%update.step)==0){setTxtProgressBar(pb, i)}
  }
  
  #------ Fechando conexao da barra de progresso 
  if(update.step!=0){close(pb)}
  
  # RESULTADO ---------------------------------------- 

  #------ Serie em ordem alfabetica com respectivo numero de faltantes (missing) 
  saida <- saida[order(saida$N_MISS,decreasing = T),]
  
  #------ Banco resultado 
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
  
  #------ Resultado
  return(saida2)
}