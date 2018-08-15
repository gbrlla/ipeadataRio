# --------------------------------------------------------- #
# |   INSTITUTO DE PESQUISA ECONOMICA APLICADA - IPEA     | #
# |                    PROJETO IPEADATA                   | #
# --------------------------------------------------------- #
# |   COORDENADOR: ERIVELTON P. GUEDES                    | #
# --------------------------------------------------------- #
# |   PROGRAMADOR: LUIZ EDUARDO S. GOMES                  | #
# --------------------------------------------------------- #
# |   CONFAZ - WEBSCRAPPING                               | #
# --------------------------------------------------------- #

# CARREGANDO PACOTES ----------------------------------------

pacotes<-c("mvtnorm","forecast","Matrix","matrixStats",
           "corpcor","DBI","RCurl","curl","XML",
           "RPostgreSQL","httr","xml2","rvest",
           "dplyr","xlsx")

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

MLD <- function(Y,period,plotar = FALSE,IC = .95,
                namostra = 10000,dataY = NULL,
                Fnivel = TRUE, Ftend = TRUE, Fsaz = TRUE, 
                priori = TRUE, logY = FALSE)
{ 
  # ------ Avisos de erro
  if(!is.vector(Y)){warning("A serie deve ser um vetor numerico")}
  if(length(Y)==1){warning("Comprimento da serie deve ser > 1")}
  if(length(Y)==0){warning("Serie vazia")}
  if(is.null(period)){warning("Periodicidade da serie deve ser informada: \n 12 - Mensal, 4 - Trimestral, 2 - Semestral, 1 - Outras.")}
  
  # ------ Usando o log da serie***
  if(logY){Y = log(Y)}
  
  # -------------------------------------------------------------- #
  # IMPORTANTE:
  # O uso do log acarreta subestimacao dos ICs 
  # Nao afirme acerca do ICs calculados
  # + informacoes: 
  # Bayesian forecasting and dynamic models (WEST & HARRISON, 1997)
  # Capitulo 10 --
  # -------------------------------------------------------------- #
  
  # ------ Ordens
  n <- 1
  T <- length(Y)
  if(!is.null(dataY) & period == 12){r <- 16}
  if(!is.null(dataY) & period == 4){r <- 8}
  if(!is.null(dataY) & period == 2){r <- 6}
  if(is.null(dataY)){r <- 5}
  
  #------ Serie temporal
  Yt <- array(NA,dim=c(n,1,T+1))
  for (t in 2:(T+1)){Yt[,,t] <- c(Y[t-1])}
  
  #------ Matriz de evolucao Gt
  Gt <- as.matrix(Matrix::bdiag(1,
                                diag(1,r-5),
                                matrix(c(1,0,1,1),2),
                                matrix(c(cos(2*pi/period),-sin(2*pi/period),
                                         sin(2*pi/period),cos(2*pi/period)),2)))
  
  #------ Matriz de descontos
  delta <- c(.99,.99,.99,.99)
  delta <- 1/sqrt(delta)
  DescMatrix <- array(NA,dim=c(r,r,T+1))
  for (t in 2:(T+1))
  {
    DescMatrix[,,t] <- as.matrix(Matrix::bdiag(delta[1],
                                               diag(delta[2],r-5),
                                               matrix(c(rep(delta[3],2^2)),2),
                                               matrix(c(rep(delta[4],2^2)),2)))
  }
  DescMatrix <- ifelse(DescMatrix>0,DescMatrix,1)
  
  #------ Fator de desconto sigma2
  delta.sig2 = .9
  
  #------ Matriz de design Ft
  Ft <- array(NA,dim=c(r,n,T+1))
  if(is.null(dataY)){for (t in 2:(T+1)){Ft[,,t] <- matrix(c(ifelse(Fnivel,1,0),
                                                            ifelse(Ftend,1,0),0,
                                                            ifelse(Fsaz,1,0),0))}}
  if(!is.null(dataY) & period == 12)
  {
    for (t in 2:(T+1))
    {
      Ft[,,t] <- matrix(c(ifelse(Fnivel,1,0),
                          ifelse(substr(dataY[t-1],6,7)=="01",1,0),
                          ifelse(substr(dataY[t-1],6,7)=="02",1,0),
                          ifelse(substr(dataY[t-1],6,7)=="03",1,0),
                          ifelse(substr(dataY[t-1],6,7)=="04",1,0),
                          ifelse(substr(dataY[t-1],6,7)=="05",1,0),
                          ifelse(substr(dataY[t-1],6,7)=="06",1,0),
                          ifelse(substr(dataY[t-1],6,7)=="07",1,0),
                          ifelse(substr(dataY[t-1],6,7)=="08",1,0),
                          ifelse(substr(dataY[t-1],6,7)=="10",1,0),
                          ifelse(substr(dataY[t-1],6,7)=="11",1,0),
                          ifelse(substr(dataY[t-1],6,7)=="12",1,0),
                          ifelse(Ftend,1,0),0,
                          ifelse(Fsaz,1,0),0))
    }
  }
  
  if(!is.null(dataY) & period == 4)
  {
    for (t in 2:(T+1))
    {
      Ft[,,t] <- matrix(c(ifelse(Fnivel,1,0),
                          ifelse(substr(dataY[t-1],6,7)=="01",1,0),
                          ifelse(substr(dataY[t-1],6,7)=="04",1,0),
                          ifelse(substr(dataY[t-1],6,7)=="10",1,0),
                          ifelse(Ftend,1,0),0,
                          ifelse(Fsaz,1,0),0))
    }
  }
  
  
  if(!is.null(dataY) & period == 2)
  {
    for (t in 2:(T+1))
    {
      Ft[,,t] <- matrix(c(ifelse(Fnivel,1,0),
                          ifelse(substr(dataY[t-1],6,7)=="07",1,0),
                          ifelse(Ftend,1,0),0,
                          ifelse(Fsaz,1,0),0))
    }
  }
  
  # ------ Inicializando os arrays
  mt <- array(data = NA,dim = c(r,1,T+1))
  Ct <- array(data = NA,dim = c(r,r,T+1))
  at <- array(data = NA,dim = c(r,1,T+1))
  Rt <- array(data = NA,dim = c(r,r,T+1))
  ft <- array(data = NA,dim = c(n,1,T+1))
  ft2 <- array(data = NA,dim = c(n,1,T+1))
  ft2.LI <- array(data = NA,dim = c(n,1,T+1))
  ft2.LS <- array(data = NA,dim = c(n,1,T+1))
  Qt <- array(data = NA,dim = c(n,n,T+1))
  Qt2 <- array(data = NA,dim = c(n,n,T+1))
  At <- array(data = NA,dim = c(r,n,T+1))
  et <- array(data = NA,dim = c(n,1,T+1))
  ht <- array(data = NA,dim = c(r,1,T+1))
  Ht <- array(data = NA,dim = c(r,r,T+1))
  nt2 <- rep(NA,T+1)
  St2 <- rep(NA,T+1)
  
  # ------ Distribuicoes a priori
  mtPrior <- rep(0,r)
  CtPrior <- diag(1000,r)
  ntPrior <- 1
  StPrior <- 1000
  
  if(priori)
  {
    mt[,,1] <- mtPrior
  } else {
    mt[,,1] <- c(as.numeric(na.exclude(Y))[1],rep(0,r-1))
  }
  
  if(!logY)
  {
    Ct[,,1] <- CtPrior
    St <- StPrior
  } else {
    Ct[,,1] <- diag(1,r)
    St <- 1
  }
  
  nt <- ntPrior
  
  # ------ Filtro de Kalman ###
  for (t in 2:(T+1))
  {
    # ------ Priori em t
    at[,,t] <- Gt%*%mt[,,t-1]
    Rt[,,t] <- as.matrix(Matrix::forceSymmetric(DescMatrix[,,t]*(Gt%*%Ct[,,t-1]%*%t(Gt))*DescMatrix[,,t]))
    Rt[,,t] <- as.matrix(Matrix::forceSymmetric(corpcor::make.positive.definite(Rt[,,t])))
    
    # ------ Previsao 1 passo a frente
    ft[,,t] <- t(Ft[,,t])%*%at[,,t]
    Qt[,,t] <- as.matrix(Matrix::forceSymmetric(t(Ft[,,t])%*%Rt[,,t]%*%Ft[,,t] + St[t-1]*diag(1,n)))
    Qt[,,t] <- as.matrix(Matrix::forceSymmetric(corpcor::make.positive.definite(Qt[,,t])))
    
    # ------ Posteriori em t
    if(!is.na(Yt[,,t]))
    {
      At[,,t] <- Rt[,,t]%*%Ft[,,t]%*%Matrix::solve(Qt[,,t])
      et[,,t] <- Yt[,,t] - ft[,,t]
      mt[,,t] <- at[,,t] + (At[,,t]%*%as.matrix(et[,,t]))
      nt[t] <- delta.sig2*nt[t-1] + 1
      St[t] <- (1/nt[t])*((delta.sig2*St[t-1]*nt[t-1]) + 
                            ((1/n)*St[t-1]*(t(et[,,t])%*%Matrix::solve(Qt[,,t])%*%et[,,t])))           
      Ct[,,t] <- as.matrix(Matrix::forceSymmetric((St[t]/St[t-1])*(Rt[,,t] - (At[,,t]%*%as.matrix(Qt[,,t])%*%t(At[,,t])))))
      Ct[,,t] <- as.matrix(Matrix::forceSymmetric(corpcor::make.positive.definite(Ct[,,t])))
    } else {
      At[,,t] <- NA
      et[,,t] <- NA
      mt[,,t] <- at[,,t]
      nt[t] <- nt[t-1]
      St[t] <- St[t-1]          
      Ct[,,t] <- Rt[,,t]
    }
  }
  
  # ------ Analise Retrospectiva ###
  for (t in (T+1):1)
  {
    if(t == T+1)
    {
      ht[,,t] <- mt[,,t]
      Ht[,,t] <- Ct[,,t]
      nt2[t] <- nt[t]
      St2[t] <- St[t]
    } else {
      Maux <- Ct[,,t]%*%Gt%*%solve(Rt[,,t+1])
      ht[,,t] <- mt[,,t] + (Maux%*%(ht[,,t+1]-at[,,t+1]))
      Ht[,,t] <- as.matrix(Matrix::forceSymmetric(Ct[,,t]-(Maux%*%Gt%*%Ct[,,t])))
      Ht[,,t] <- as.matrix(Matrix::forceSymmetric(corpcor::make.positive.definite(Ht[,,t])))
      nt2[t] <- (((1-delta.sig2)*(nt[t])) + (delta.sig2*nt2[t+1]))
      St2[t] <- 1/(((1-delta.sig2)*(1/St[t])) + (delta.sig2*(1/St2[t+1])))
    }
  }
  
  # ------ Ajuste Suavizado ###
  for (t in 2:(T+1))
  {
    # ------ Ajuste
    ft2[,,t] <- t(Ft[,,t])%*%Gt%*%ht[,,t]
    Qt2[,,t] <- as.matrix(Matrix::forceSymmetric(corpcor::make.positive.definite(t(Ft[,,t])%*%(DescMatrix[,,t]*(Gt%*%Ht[,,t-1]%*%t(Gt))*DescMatrix[,,t])%*%Ft[,,t] + St2[t]*diag(1,n))))
    compIC <- mvtnorm::rmvt(n = namostra,sigma = as.matrix(Qt2[,,t]),
                            df = nt2[t],delta = ft2[,,t])
    ft2.LI[,,t] <- matrixStats::colQuantiles(x = compIC,probs = (1-IC)/2)
    ft2.LS[,,t] <- matrixStats::colQuantiles(x = compIC,probs = 1-((1-IC)/2))
  }
  
  # ------ Previsao 1 passo a frente (fora da amostra) ###
  # ------ Priori em T
  at.k <- Gt%*%mt[,,T]
  Rt.k <- as.matrix(Matrix::forceSymmetric(DescMatrix[,,T]*(Gt%*%Ct[,,T]%*%t(Gt))*DescMatrix[,,T]))
  Rt.k <- as.matrix(Matrix::forceSymmetric(corpcor::make.positive.definite(Rt.k)))
  
  # ------ Previsao 1 passo a frente
  ft.k <- t(Ft[,,T])%*%at.k
  Qt.k <- as.matrix(Matrix::forceSymmetric(t(Ft[,,T])%*%Rt.k%*%Ft[,,T] + St[T]*diag(1,n)))
  Qt.k <- as.matrix(Matrix::forceSymmetric(corpcor::make.positive.definite(Qt.k)))
  compIC2 <- mvtnorm::rmvt(n = namostra,sigma = as.matrix(Qt.k),
                           df = nt2[T],delta = ft.k)
  ftk.LI <- matrixStats::colQuantiles(x = compIC2,probs = (1-IC)/2)
  ftk.LS <- matrixStats::colQuantiles(x = compIC2,probs = 1-((1-IC)/2))
  
  # ------ Plotando
  if(plotar)
  {
    #------ Grafico dinamico
    print(dygraphs::dySeries(dygraph = dygraphs::dygraph(data = data.frame(x = 1:length(Y), ft.LI = ft2.LI[,,-1],ft = ft2[,,-1], ft.LS = ft2.LS[,,-1],Observado = Yt[,,-1]),
                                                         main = paste("IC",round(IC*100),"%")),
                             name = c("ft.LI","ft","ft.LS"),label = "Previsao"))
  }
  
  # ------ Resultado
  if(!logY)
  {
    resultado <- data.frame(obs = round(Y,0),
                            prev = round(ft2[,,-1],0),
                            prev.LI = round(ft2.LI[,,-1],0),
                            prev.LS = round(ft2.LS[,,-1],0),
                            indic = ifelse(Y < ft2.LI[,,-1] |
                                             Y > ft2.LS[,,-1],1,0),
                            prevk1 = c(round(ft.k,0),rep(NA,length(Y)-1)),
                            prevk1.LI = c(round(ftk.LI,0),rep(NA,length(Y)-1)),
                            prevk1.LS = c(round(ftk.LS,0),rep(NA,length(Y)-1)))
  } else {
    resultado <- data.frame(obs = round(exp(Y),0),
                            prev = round(exp(ft2[,,-1]),0),
                            prev.LI = round(exp(ft2.LI[,,-1]),0),
                            prev.LS = round(exp(ft2.LS[,,-1]),0),
                            indic = ifelse(exp(Y) < exp(ft2.LI[,,-1]) |
                                             exp(Y) > exp(ft2.LS[,,-1]),1,0),
                            prevk1 = c(round(exp(ft.k),0),rep(NA,length(Y)-1)),
                            prevk1.LI = c(round(exp(ftk.LI),0),rep(NA,length(Y)-1)),
                            prevk1.LS = c(round(exp(ftk.LS),0),rep(NA,length(Y)-1)))
  }
  
  return(resultado)
}

CONFAZ12WB <- function(gerarGen = TRUE)
{
  # WEBSCRAPPING ----------------------------------------------
  
  #------ Codigo dos paises (DB interno)
  codterrit <- codterritCONFAZ12
  
  #------ Desligando notacao cientifica
  options(scipen=999)
  
  #------ Metadados utilizados pelo site
  ano <- 1996:(as.POSIXlt(Sys.Date())$year+1900)
  tipo <- c("valores_correntes")
  conta <- c("icms_total",
             "outros_tributos_ipva",
             "outros_tributos_itcd",
             "outros_tributos_taxas",
             "outros_tributos_outros")
  metadados <- expand.grid(ano,tipo,conta)
  
  #------ Inicializando DF
  GERADO <- data.frame(NULL)
  
  #------ Atualizacao da barra de progresso
  update.step <- max(5, floor(nrow(metadados)/100))
  
  #------ Texto informativo
  message("Raspando dados de https://www.confaz.fazenda.gov.br/legislacao/boletim-do-icms")
  
  #------ Barra de progresso
  pb <- utils::txtProgressBar(max = nrow(metadados), style = 3)
  
  #------ Raspando ###
  for (i in 1:nrow(metadados))
  {
    Sys.sleep(0.1)
    
    #------ URL
    httr::set_config(httr::config(ssl_verifypeer = 0L))
    url <- 'https://www.confaz.fazenda.gov.br/legislacao/boletim-do-icms'
    
    #------ Parametros
    params <- list(`form.submitted` = "1",
                   `ano` = paste(metadados[i,1]),
                   `tipo_consulta` = paste(metadados[i,2]),
                   `conta` = paste(metadados[i,3]),
                   `form.button.Search` = "Buscar")
    
    #------ Consulta
    tabela <- rvest::html_table(xml2::read_html(httr::POST(url,
                                                           body = params,
                                                           encode = 'form')))
    
    #------ Organizando
    ## Remove as colunas 1 a 2
    ## Mantem as colunas 3 a 14
    if ((metadados[i,3]=="icms_total")|
        (metadados[i,3]=="outros_tributos_ipva")|
        (metadados[i,3]=="outros_tributos_itcd"))
    {exclinhas <- c(1:5,13,23,28,32)} else {exclinhas <- c(1:36)}
    
    #------ DF auxiliar
    aux <- data.frame(valdata=as.character(seq(as.Date(paste0(metadados[i,1],"-01-15"), 
                                                       origin="1900-01-01"),
                                               as.Date(paste0(metadados[i,1],"-12-15"), 
                                                       origin="1900-01-01"),
                                               by="1 month")),
                      t(tabela[[1]][-exclinhas,3:14]),row.names=NULL)
    names(aux)[-1] <- tabela[[1]][-exclinhas,1]
    
    #------ Organizando texto - Removendo acentos e colocando em maiusculo 
    names(aux) <- toupper(iconv(x = names(aux),from = "UTF-8",to="ASCII//TRANSLIT"))
    
    #------ Substituindo pelo metadado correspondente
    if (metadados[i,3]=="icms_total")
    {
      codterrit2 <- subset(x = codterrit, subset = codterrit$conta == "icms_total")
      
      #------ Alterando label
      names(aux) <- factor(x = names(aux),
                               levels = c("VALDATA",as.character(codterrit2$territorio)),
                               labels = c("VALDATA",as.character(codterrit2$codtroll)))
    }
    
    if (metadados[i,3]=="outros_tributos_ipva")
    {
      codterrit2 <- subset(x = codterrit, subset = codterrit$conta == "outros_tributos_ipva")
      
      #------ Alterando label
      names(aux) <- factor(x = names(aux),
                           levels = c("VALDATA",as.character(codterrit2$territorio)),
                           labels = c("VALDATA",as.character(codterrit2$codtroll)))
    }
    if (metadados[i,3]=="outros_tributos_itcd")
    {
      codterrit2 <- subset(x = codterrit, subset = codterrit$conta == "outros_tributos_itcd")
      
      #------ Alterando label
      names(aux) <- factor(x = names(aux),
                           levels = c("VALDATA",as.character(codterrit2$territorio)),
                           labels = c("VALDATA",as.character(codterrit2$codtroll)))
    }
    if (metadados[i,3]=="outros_tributos_taxas"){names(aux) <- c("VALDATA","CONFAZ12_TAXAS12")}
    if (metadados[i,3]=="outros_tributos_outros"){names(aux) <- c("VALDATA","CONFAZ12_OUT12")}
    
    #------ Padrao Postgree
    for (l in 2:ncol(aux))
    {
      GERADO <- rbind(GERADO,data.frame(serid=as.integer(NA),
                                        valdata=as.Date(aux[,1], origin = "1900-01-01"),
                                        terid=as.integer(1),
                                        valvalor=aux[,l],
                                        ocrid=as.integer(NA),
                                        sercodigotroll=names(aux)[l],
                                        atualizdata=Sys.time(),row.names = NULL))
    }

    #------ Barra de progresso na tela
    utils::setTxtProgressBar(pb, i)
  }
  
  #------ Fechando conexao da barra de progresso
  close(pb)
  
  #------ Removendo texto dos valores
  GERADO$valvalor <- gsub("[.]", "", GERADO$valvalor)
  GERADO$valvalor <- gsub("[*]", "", GERADO$valvalor)
  GERADO$valvalor <- ifelse(GERADO$valvalor==0,NA,as.numeric(GERADO$valvalor))
  GERADO <- subset(GERADO,is.na(GERADO$valvalor)==F)
  GERADO$valdata <- as.Date(GERADO$valdata, origin = "1900-01-01")
  GERADO$sercodigotroll <- as.character(GERADO$sercodigotroll)
  GERADO <- GERADO[order(GERADO$sercodigotroll),]
  
  #------ Eliminando objetos 
  rm(aux,metadados,ano,conta,exclinhas,i,l,params,tabela,tipo,url,codterrit,codterrit2)
  
  # AVALIANDO VALORES -----------------------------------------
  
  #------ Planilha Generica
  GENERICA <- data.frame(VALDATA = unique(GERADO$valdata))
  sercod <- unique(GERADO$sercodigotroll)
  
  for (i in 1:length(sercod))
  {
    #------ Subset da serie
    Aux.Y <- subset(GERADO,GERADO$sercodigotroll == sercod[i])[,c(2,4)]
    names(Aux.Y)[1] = "VALDATA"
    
    #------ Montando a Generica
    GENERICA <- merge(x = GENERICA,y = Aux.Y,by = "VALDATA",all = TRUE)
    names(GENERICA)[i+1] <- sercod[i]
  }
  
  #------ Removendo possiveis valores negativos
  GENERICA[,-1] <- abs(GENERICA[,-1])
  
  #------ Salvando valores antigos para comparacao
  GENERICA.OLD <- GENERICA
  
  #------ Texto informativo
  message("Avaliando valores")
  
  #------ Atualizacao da barra de progresso
  update.step <- max(5, floor(length(sercod)/100))
  
  #------ Barra de progresso
  pb <- utils::txtProgressBar(max = length(sercod), style = 3)
  
  #------ Series de totais que nao serao corrigidas momentaneamente
  sercod.tot <- c("CONFAZ12_ICMSN12","CONFAZ12_IPVA12","CONFAZ12_ITCD12")
  
  #------ Meses como character
  meses <- c(paste0("0",1:9),"10","11","12")
  
  #------ Verificando cada serie
  for (i in 1:length(sercod))
  {
    if(sum(sercod[i]==sercod.tot)==0)
    {
      #------ Subset da serie
      Aux.Y <- data.frame(VALDATA = GENERICA$VALDATA, valvalor = GENERICA[,i+1],
                          N = 1:nrow(GENERICA))
      
      #------ Verificacao por mes
      for (m in meses)
      {
        Aux.Y2 <- subset(Aux.Y, substr(Aux.Y$VALDATA,6,7)==m)
        y <- Aux.Y2$valvalor
        A <- MLD(Y = y,period = 12,Fsaz = FALSE,priori = TRUE,IC = .9,logY = TRUE)
        A <- A[,1:5]
        B <- MLD(Y = rev(y),period = 12,Fsaz = FALSE,priori = TRUE,IC = .9,logY=TRUE)[length(y):1,]
        B <- B[,1:5]
        
        #------ Removendo os negativos
        A$prev <- ifelse(A$prev<0,0,A$prev)
        B$prev <- ifelse(B$prev<0,0,B$prev)
        A$prev.LI <- ifelse(A$prev.LI<0,0,A$prev.LI)
        B$prev.LI <- ifelse(B$prev.LI<0,0,B$prev.LI)
        
        #------ Comparativo
        C <- data.frame(A,B,IND = A$indic + B$indic,N = 1:length(y))
        D <- subset(C,C$IND>0)
        if(nrow(D)>0)
        {
          for (j in 1:nrow(D))
          {
            if(!is.na(D$obs[j]))
            {
              #------ Previsao 1 passo a frente (fora da amostra)
              if(D$N[j] >= round(length(y)/2,0))
              {
                prevMLD <- as.numeric(na.exclude(MLD(Y = y[1:(D$N[j]-1)],period = 12,Fsaz = FALSE,priori = FALSE,logY = TRUE,IC = .9)$prevk1))
              } else {
                prevMLD <- as.numeric(na.exclude(MLD(Y = rev(y[(D$N[j]+1):length(y)]),period = 12,Fsaz = FALSE,priori = TRUE,logY = TRUE,IC = .9)$prevk1))
              }
              prevMLD <- max(1,prevMLD)
              
              #------ Valores para avaliar
              opc <- c(D$obs[j]*1000,D$obs[j]*100,D$obs[j]*10,D$obs[j],
                       D$obs[j]/10,D$obs[j]/100,D$obs[j]/1000)
              
              #------ Erro absoluto para as previsoes
              erro <- c(abs((D$obs[j]*1000-prevMLD)),
                        abs((D$obs[j]*100-prevMLD)),
                        abs((D$obs[j]*10-prevMLD)),
                        abs((D$obs[j]-prevMLD)),
                        abs((D$obs[j]/10-prevMLD)),
                        abs((D$obs[j]/100-prevMLD)),
                        abs((D$obs[j]/1000-prevMLD)))
              
              #------ Atualizacao do valor
              y[D$N[j]] <- max(1,trunc(opc[which.min(erro)]))
            }
          }
        }
        
        #------ Substituindo valor
        Aux.Y$valvalor[Aux.Y2$N] <- y
      }
      
      if(substr(sercod[i],10,13)=="IPVA")
      {
        #------ Verificacao completa
        y <- Aux.Y$valvalor
        A <- MLD(Y = y,period = 12,dataY = Aux.Y$VALDATA,priori = FALSE,logY = TRUE,IC = .9)
        A <- A[,1:5]
        B <- MLD(Y = rev(y),period = 12,dataY = Aux.Y$VALDATA,logY = TRUE,IC = .9)[length(y):1,]
        B <- B[,1:5]
        
        #------ Removendo os negativos
        A$prev <- ifelse(A$prev<0,0,A$prev)
        B$prev <- ifelse(B$prev<0,0,B$prev)
        A$prev.LI <- ifelse(A$prev.LI<0,0,A$prev.LI)
        B$prev.LI <- ifelse(B$prev.LI<0,0,B$prev.LI)
        
        #------ Comparativo
        C <- data.frame(A,B,IND = A$indic + B$indic,N = 1:length(y))
        D <- unique(rbind(C[1,],subset(C,C$IND==2),C[nrow(C),]))
        if(nrow(D)>0)
        {
          for (j in 1:nrow(D))
          {
            if(!is.na(D$obs[j]))
            {
              #------ Previsao 1 passo a frente (fora da amostra)
              if(D$N[j] >= round(length(y)/2,0))
              {
                prevMLD <- as.numeric(na.exclude(MLD(Y = y[1:(D$N[j]-1)],period = 12,dataY = Aux.Y$VALDATA,priori = FALSE,logY = TRUE,IC = .9)$prevk1))
              } else {
                prevMLD <- as.numeric(na.exclude(MLD(Y = rev(y[(D$N[j]+1):length(y)]),period = 12,dataY = Aux.Y$VALDATA,priori = FALSE,logY = TRUE,IC = .9)$prevk1))
              }
              prevMLD <- max(1,prevMLD)
              
              #------ Valores para avaliar
              opc <- c(D$obs[j]*1000,D$obs[j]*100,D$obs[j]*10,D$obs[j],
                       D$obs[j]/10,D$obs[j]/100,D$obs[j]/1000)
              
              #------ Erro absoluto para as previsoes
              erro <- c(abs((D$obs[j]*1000-prevMLD)),
                        abs((D$obs[j]*100-prevMLD)),
                        abs((D$obs[j]*10-prevMLD)),
                        abs((D$obs[j]-prevMLD)),
                        abs((D$obs[j]/10-prevMLD)),
                        abs((D$obs[j]/100-prevMLD)),
                        abs((D$obs[j]/1000-prevMLD)))
              
              #------ Atualizacao do valor@
              y[D$N[j]] <- trunc(opc[which.min(erro)])
            }
          }
        }
        
        #------ Substituindo valor
        Aux.Y$valvalor <- y
        
      } else {
        #------ Verificacao completa
        y <- Aux.Y$valvalor
        A <- MLD(Y = y,period = 12,priori = FALSE,logY = TRUE,Fsaz = FALSE,IC = .9)
        A <- A[,1:5]
        B <- MLD(Y = rev(y),period = 12,priori = FALSE,logY = TRUE,Fsaz = FALSE,IC = .9)[length(y):1,]
        B <- B[,1:5]
        
        #------ Removendo os negativos
        A$prev <- ifelse(A$prev<0,0,A$prev)
        B$prev <- ifelse(B$prev<0,0,B$prev)
        A$prev.LI <- ifelse(A$prev.LI<0,0,A$prev.LI)
        B$prev.LI <- ifelse(B$prev.LI<0,0,B$prev.LI)
        
        #------ Comparativo
        C <- data.frame(A,B,IND = A$indic + B$indic,N = 1:length(y))
        D <- unique(rbind(C[1,],subset(C,C$IND==2),C[nrow(C),]))
        if(nrow(D)>0)
        {
          for (j in 1:nrow(D))
          {
            if(!is.na(D$obs[j]))
            {
              #------ Previsao 1 passo a frente (fora da amostra)
              if(D$N[j] >= round(length(y)/2,0))
              {
                prevMLD <- as.numeric(na.exclude(MLD(Y = y[1:(D$N[j]-1)],period = 12,priori = FALSE,logY = TRUE,Fsaz = FALSE,IC = .9)$prevk1))
              } else {
                prevMLD <- as.numeric(na.exclude(MLD(Y = rev(y[(D$N[j]+1):length(y)]),period = 12,priori = TRUE,logY = TRUE,Fsaz = FALSE,IC = .9)$prevk1))
              }
              prevMLD <- max(1,prevMLD)
              
              #------ Valores para avaliar
              opc <- c(D$obs[j]*1000,D$obs[j]*100,D$obs[j]*10,D$obs[j],
                       D$obs[j]/10,D$obs[j]/100,D$obs[j]/1000)
              
              #------ Erro absoluto para as previsoes
              erro <- c(abs((D$obs[j]*1000-prevMLD)),
                        abs((D$obs[j]*100-prevMLD)),
                        abs((D$obs[j]*10-prevMLD)),
                        abs((D$obs[j]-prevMLD)),
                        abs((D$obs[j]/10-prevMLD)),
                        abs((D$obs[j]/100-prevMLD)),
                        abs((D$obs[j]/1000-prevMLD)))
              
              #------ Atualizacao do valor
              y[D$N[j]] <- trunc(opc[which.min(erro)])
            }
          }
        }
        
        #------ Substituindo valor
        Aux.Y$valvalor <- y
      }
      
      #------ Atualizando valores
      GENERICA[,i+1] <- Aux.Y$valvalor
    }
    
    #------ Barra de progresso na tela
    utils::setTxtProgressBar(pb, i)
  }
  
  #------ Fechando conexao da barra de progresso
  close(pb)
  
  #------ Montando series nacionais ###
  
  ## CONFAZ12_ICMSN12
  GENERICA[,15] <- Matrix::rowSums(x = GENERICA[,c(2:14,16:29)],na.rm = TRUE)
  
  ## CONFAZ12_IPVA12
  GENERICA[,30] <- Matrix::rowSums(x = GENERICA[,31:57],na.rm = TRUE)
  
  ## CONFAZ12_ITCD12
  GENERICA[,58] <- Matrix::rowSums(x = GENERICA[,59:85],na.rm = TRUE)
  
  #------ Removendo potenciais zeros
  for (j in 2:ncol(GENERICA))
  {
    GENERICA[,j] <- ifelse(test = GENERICA[,j]== 0,yes = NA,no = GENERICA[,j])
  }
  
  #------ Eliminando objetos 
  rm(A,B,C,D,Aux.Y,Aux.Y2,erro,i,j,opc,y,prevMLD,m,meses,sercod.tot)
    
  # GERANDO GRAFICOS --------------------------------------
  
  #------ Texto informativo
  message("Salvando graficos em //Srjn3/area_corporativa/Projeto_IPEADATA/Geral/PacoteIpeadataRio/CONFAZ12 - Graficos de Verificacao")
  
  #------ Atualizacao da barra de progresso
  update.step <- max(5, floor(ncol(GENERICA)/100))
  
  #------ Barra de progresso
  pb <- utils::txtProgressBar(max = ncol(GENERICA), style = 3)
  
  for (j in 2:ncol(GENERICA))
  {
    if(substr(names(GENERICA)[j],10,13)=="IPVA")
    {
      #------ Salvando em formato .png
      grDevices::png(paste0("//Srjn3/area_corporativa/Projeto_IPEADATA/Geral/PacoteIpeadataRio/CONFAZ12 - Graficos de Verificacao/",names(GENERICA)[j],".png"),width = 640, height = 480)
      graf.aux <- MLD(Y = GENERICA[,j],period = 12,dataY = GENERICA$VALDATA,priori = FALSE,IC = .9)
      graf.aux$prev <- ifelse(graf.aux$prev<0,0,graf.aux$prev)
      graf.aux$prev.LI <- ifelse(graf.aux$prev.LI<0,0,graf.aux$prev.LI)
      par(mar = c(4,4,.5,.5))
      plot(1,type="n",
           ylim=c(max(0,min(graf.aux$prev.LI)),max(graf.aux$prev.LS)),
           xlim=c(0,nrow(graf.aux)),ylab=names(GENERICA)[j],
           xlab="Tempo")
      color_transparent <- adjustcolor("blue", alpha.f = 0.3)
      polygon(c(rev(1:nrow(graf.aux)),1:nrow(graf.aux)),c(rev(graf.aux$prev.LS),graf.aux$prev.LI),col=color_transparent,border=NA)
      points(GENERICA.OLD[,j],pch=19,cex=1,col=3,t="o",lwd=3)
      points(graf.aux$obs,pch=19,cex=1,col=2,t="o",lwd=2)
      lines(graf.aux$prev,col=4,lwd=2)
      legend("topleft",legend = c("Estimativa","Observacao Corrigida","Observacao Nao Corrigida"),
             pch=c(NA,19,19),lwd=c(2,NA,NA),col=c(4,2,3),bty="n",horiz = T)
      dev.off() 
    } else {
      #------ Salvando em formato .png
      grDevices::png(paste0("//Srjn3/area_corporativa/Projeto_IPEADATA/Geral/PacoteIpeadataRio/CONFAZ12 - Graficos de Verificacao/",names(GENERICA)[j],".png"),width = 640, height = 480)
      graf.aux <- MLD(Y = GENERICA[,j],period = 12,Fsaz = FALSE,priori = FALSE,IC = .9)
      graf.aux$prev <- ifelse(graf.aux$prev<0,0,graf.aux$prev)
      graf.aux$prev.LI <- ifelse(graf.aux$prev.LI<0,0,graf.aux$prev.LI)
      par(mar = c(4,4,.5,.5))
      plot(1,type="n",
           ylim=c(max(0,min(graf.aux$prev.LI)),max(graf.aux$prev.LS)),
           xlim=c(0,nrow(graf.aux)),ylab=names(GENERICA)[j],
           xlab="Tempo")
      color_transparent <- adjustcolor("blue", alpha.f = 0.3)
      polygon(c(rev(1:nrow(graf.aux)),1:nrow(graf.aux)),c(rev(graf.aux$prev.LS),graf.aux$prev.LI),col=color_transparent,border=NA)
      points(GENERICA.OLD[,j],pch=19,cex=1,col=3,t="o",lwd=3)
      points(graf.aux$obs,pch=19,cex=1,col=2,t="o",lwd=2)
      lines(graf.aux$prev,col=4,lwd=2)
      legend("topleft",legend = c("Estimativa","Observacao Corrigida","Observacao Nao Corrigida"),
             pch=c(NA,19,19),lwd=c(2,NA,NA),col=c(4,2,3),bty="n",horiz = T)
      dev.off() 
    }
    #------ Barra de progresso na tela
    utils::setTxtProgressBar(pb, j)
  }
  
  #------ Fechando conexao da barra de progresso
  close(pb)
  
  #------ Eliminando objetos 
  rm(color_transparent,j,pb,update.step,graf.aux,sercod)
  
  #------ Comparando valores
  VALORES.BASE <- GENERICAverif(nomes = names(GENERICA)[-1])
  
  #------ Organizando data
  VALORES.BASE$VALDATA <- VALORES.BASE$VALDATA + 14
  
  #------ Base auxiliar
  VALORES.BASE2 <- merge(x = VALORES.BASE,y = GENERICA,by = "VALDATA")[,1:ncol(GENERICA)]
  VALORES.BASE3 <- merge(x = GENERICA,y = VALORES.BASE,by = "VALDATA")[,1:ncol(GENERICA)]
  
  #------ Atualizar?
  atualizar <- FALSE
  if(nrow(GENERICA)>nrow(VALORES.BASE2)){atualizar <- TRUE}
  if(nrow(GENERICA)==nrow(VALORES.BASE2))
  {
    if(sum(VALORES.BASE2[,-1]!=GENERICA[,-1],na.rm = TRUE)>0){atualizar <- TRUE}
  }

  if(gerarGen & atualizar)
  {
    #------ Texto informativo
    message("Exportando planilha de atualizacao para //Srjn3/area_corporativa/Projeto_IPEADATA/ETL/Generica")
    
    # SALVANDO GENERICA --------------------------------------
    
    #------ Exportando xls
    xlsx::write.xlsx(x = GENERICA,
                     file = "//Srjn3/area_corporativa/Projeto_IPEADATA/ETL/Generica/GENERICA_CONFAZ12.xls",
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
    autolog[r,] <- c(as.character(Sys.time()),Sys.getenv("USERNAME"),"GENERICA_CONFAZ12")
    
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
  cat(paste("Relatorio do banco CONFAZ12 em",Sys.Date(),"\n"))
  cat("RESUMO \n")
  cat(paste("Numero de alteracoes ..................",sum(GENERICA[,-1] != GENERICA.OLD[,-1],na.rm = TRUE)),"\n")
  cat(paste("Numero de revisoes ....................",sum(VALORES.BASE2[,-1]!=VALORES.BASE3[,-1],na.rm = TRUE)),"\n")
  cat("\n")
  
  #------ Resultado
  return(GENERICA)
}

# --------------------------------------------------------- #
# AREA DE TESTE ---------------------------------------- 
a <- Sys.time()
A <- CONFAZ12WB()
Sys.time()-a
# --------------------------------------------------------- #

