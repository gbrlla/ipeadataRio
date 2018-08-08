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

CONFAZ12WB <- function(gerarGen = TRUE)
{
  # WEBSCRAPPING ----------------------------------------------
  
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
    
    #------ Substituindo pelo metadado correspondente
    if (metadados[i,3]=="icms_total")
    {
      GERADO$sercodigotroll <- gsub("Mato Grosso do Sul","CONFAZ12_ICMSMS12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Acre","CONFAZ12_ICMSAC12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Amazonas","CONFAZ12_ICMSAM12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Pará","CONFAZ12_ICMSPA12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Rondônia","CONFAZ12_ICMSRO12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Amapá","CONFAZ12_ICMSAP12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Roraima","CONFAZ12_ICMSRR12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Tocantins","CONFAZ12_ICMSTO12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Maranhão","CONFAZ12_ICMSMA12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Piauí","CONFAZ12_ICMSPI12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Ceará","CONFAZ12_ICMSCE12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Rio Grande do Norte","CONFAZ12_ICMSRN12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Paraíba","CONFAZ12_ICMSPB12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Pernambuco","CONFAZ12_ICMSPE12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Alagoas","CONFAZ12_ICMSAL12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Sergipe","CONFAZ12_ICMSSE12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Bahia","CONFAZ12_ICMSBA12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Minas Gerais","CONFAZ12_ICMSMG12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Espírito Santo","CONFAZ12_ICMSES12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Rio de Janeiro","CONFAZ12_ICMSRJ12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("São Paulo","CONFAZ12_ICMSSP12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Paraná","CONFAZ12_ICMSPR12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Santa Catarina","CONFAZ12_ICMSSC12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Rio Grande do Sul","CONFAZ12_ICMSRS12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Mato Grosso","CONFAZ12_ICMSMT12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Goiás","CONFAZ12_ICMSGO12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Distrito Federal","CONFAZ12_ICMSDF12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("BRASIL","CONFAZ12_ICMSN12", fixed = T, GERADO$sercodigotroll)
    }
    if (metadados[i,3]=="outros_tributos_ipva")
    {
      GERADO$sercodigotroll <- gsub("Mato Grosso do Sul","CONFAZ12_IPVAMS12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Acre","CONFAZ12_IPVAAC12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Amazonas","CONFAZ12_IPVAAM12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Pará","CONFAZ12_IPVAPA12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Rondônia","CONFAZ12_IPVARO12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Amapá","CONFAZ12_IPVAAP12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Roraima","CONFAZ12_IPVARR12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Tocantins","CONFAZ12_IPVATO12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Maranhão","CONFAZ12_IPVAMA12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Piauí","CONFAZ12_IPVAPI12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Ceará","CONFAZ12_IPVACE12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Rio Grande do Norte","CONFAZ12_IPVARN12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Paraíba","CONFAZ12_IPVAPB12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Pernambuco","CONFAZ12_IPVAPE12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Alagoas","CONFAZ12_IPVAAL12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Sergipe","CONFAZ12_IPVASE12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Bahia","CONFAZ12_IPVABA12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Minas Gerais","CONFAZ12_IPVAMG12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Espírito Santo","CONFAZ12_IPVAES12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Rio de Janeiro","CONFAZ12_IPVARJ12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("São Paulo","CONFAZ12_IPVASP12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Paraná","CONFAZ12_IPVAPR12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Santa Catarina","CONFAZ12_IPVASC12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Rio Grande do Sul","CONFAZ12_IPVARS12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Mato Grosso","CONFAZ12_IPVAMT12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Goiás","CONFAZ12_IPVAGO12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Distrito Federal","CONFAZ12_IPVADF12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("BRASIL","CONFAZ12_IPVA12", fixed = T, GERADO$sercodigotroll)
    }
    if (metadados[i,3]=="outros_tributos_itcd")
    {
      GERADO$sercodigotroll <- gsub("Mato Grosso do Sul","CONFAZ12_ITCDMS12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Acre","CONFAZ12_ITCDAC12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Amazonas","CONFAZ12_ITCDAM12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Pará","CONFAZ12_ITCDPA12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Rondônia","CONFAZ12_ITCDRO12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Amapá","CONFAZ12_ITCDAP12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Roraima","CONFAZ12_ITCDRR12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Tocantins","CONFAZ12_ITCDTO12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Maranhão","CONFAZ12_ITCDMA12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Piauí","CONFAZ12_ITCDPI12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Ceará","CONFAZ12_ITCDCE12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Rio Grande do Norte","CONFAZ12_ITCDRN12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Paraíba","CONFAZ12_ITCDPB12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Pernambuco","CONFAZ12_ITCDPE12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Alagoas","CONFAZ12_ITCDAL12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Sergipe","CONFAZ12_ITCDSE12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Bahia","CONFAZ12_ITCDBA12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Minas Gerais","CONFAZ12_ITCDMG12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Espírito Santo","CONFAZ12_ITCDES12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Rio de Janeiro","CONFAZ12_ITCDRJ12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("São Paulo","CONFAZ12_ITCDSP12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Paraná","CONFAZ12_ITCDPR12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Santa Catarina","CONFAZ12_ITCDSC12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Rio Grande do Sul","CONFAZ12_ITCDRS12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Mato Grosso","CONFAZ12_ITCDMT12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Goiás","CONFAZ12_ITCDGO12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("Distrito Federal","CONFAZ12_ITCDDF12", fixed = T, GERADO$sercodigotroll)
      GERADO$sercodigotroll <- gsub("BRASIL","CONFAZ12_ITCD12", fixed = T, GERADO$sercodigotroll)
    }
    if (metadados[i,3]=="outros_tributos_taxas"){GERADO$sercodigotroll <- gsub("BRASIL","CONFAZ12_TAXAS12", fixed=T, GERADO$sercodigotroll)}
    if (metadados[i,3]=="outros_tributos_outros"){GERADO$sercodigotroll <- gsub("BRASIL","CONFAZ12_OUT12", fixed=T, GERADO$sercodigotroll)}
    
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
  rm(aux,metadados,ano,conta,exclinhas,i,l,params,tabela,tipo,url)
  
  # AVALIANDO VALORES -----------------------------------------
  
  #------ Planilha Generica
  GENERICA <- data.frame(valdata = unique(GERADO$valdata))
  sercod <- unique(GERADO$sercodigotroll)
  
  for (i in 1:length(sercod))
  {
    #------ Subset da serie
    Aux.Y <- subset(GERADO,GERADO$sercodigotroll == sercod[i])[,c(2,4)]
    
    #------ Montando a Generica
    GENERICA <- merge(x = GENERICA,y = Aux.Y,by = "valdata",all = TRUE)
    names(GENERICA)[i+1] <- sercod[i]
  }
  
  #------ Removendo possíveis valores negativos
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
  sercod.tot <- c("CONFAZ12_ICMSN12","CONFAZ12_IPVA12","CONFAZ12_ITCD12",
                  "CONFAZ12_OUT12","CONFAZ12_TAXAS12")
  
  #------ Meses como character
  meses <- c(paste0("0",1:9),"10","11","12")
  
  #------ Verificando cada serie
  for (i in 1:length(sercod))
  {
    if(sum(sercod[i]==sercod.tot)==0)
    {
      #------ Subset da serie
      Aux.Y <- data.frame(valdata = GENERICA$valdata, valvalor = GENERICA[,i+1],
                          N = 1:nrow(GENERICA))
      
      #------ Verificacao por mes
      for (m in meses)
      {
        Aux.Y2 <- subset(Aux.Y, substr(Aux.Y$valdata,6,7)==m)
        y <- Aux.Y2$valvalor
        A <- MLD(Y = y,period = 12,Fsaz = FALSE,priori = FALSE,IC = .9,logY = TRUE)
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
                prevMLD <- as.numeric(na.exclude(MLD(Y = y[1:(D$N[j]-1)],period = 12,Fsaz = FALSE,priori = FALSE,logY = TRUE)$prevk1))
              } else {
                prevMLD <- as.numeric(na.exclude(MLD(Y = rev(y[(D$N[j]+1):length(y)]),period = 12,Fsaz = FALSE,priori = TRUE,logY = TRUE)$prevk1))
              }
              prevMLD <- max(0,prevMLD)
              
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
        A <- MLD(Y = y,period = 12,plot = FALSE,dataY = Aux.Y$valdata,priori = FALSE,logY = TRUE)
        A <- A[,1:5]
        B <- MLD(Y = rev(y),period = 12,plot = FALSE,dataY = Aux.Y$valdata,logY = TRUE)[length(y):1,]
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
                prevMLD <- as.numeric(na.exclude(MLD(Y = y[1:(D$N[j]-1)],period = 12,dataY = Aux.Y$valdata,priori = FALSE,logY = TRUE)$prevk1))
              } else {
                prevMLD <- as.numeric(na.exclude(MLD(Y = rev(y[(D$N[j]+1):length(y)]),period = 12,dataY = Aux.Y$valdata,priori = FALSE,logY = TRUE)$prevk1))
              }
              prevMLD <- max(0,prevMLD)
              
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
        A <- MLD(Y = y,period = 12,priori = FALSE,logY = TRUE,plot = FALSE,Fsaz = FALSE)
        A <- A[,1:5]
        B <- MLD(Y = rev(y),period = 12,priori = FALSE,logY = TRUE,Fsaz = FALSE)[length(y):1,]
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
                prevMLD <- as.numeric(na.exclude(MLD(Y = y[1:(D$N[j]-1)],period = 12,priori = FALSE,logY = TRUE,Fsaz = FALSE)$prevk1))
              } else {
                prevMLD <- as.numeric(na.exclude(MLD(Y = rev(y[(D$N[j]+1):length(y)]),period = 12,priori = TRUE,logY = TRUE,Fsaz = FALSE)$prevk1))
              }
              prevMLD <- max(0,prevMLD)
              
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
    
    if(sum(sercod[i]==c("CONFAZ12_OUT12","CONFAZ12_TAXAS12"))==1)
    {
      #------ Subset da serie
      Aux.Y <- data.frame(valdata = GENERICA$valdata, valvalor = GENERICA[,i+1],
                          N = 1:nrow(GENERICA))
      #------ Verificacao completa
      y <- Aux.Y$valvalor
      A <- MLD(Y = y,period = 12,priori = FALSE,logY = TRUE)
      A <- A[,1:5]
      B <- MLD(Y = rev(y),period = 12,priori = FALSE,logY = TRUE)[length(y):1,]
      B <- B[,1:5]
      
      #------ Removendo os negativos
      A$prev <- ifelse(A$prev<0,0,A$prev)
      B$prev <- ifelse(B$prev<0,0,B$prev)
      A$prev.LI <- ifelse(A$prev.LI<0,0,A$prev.LI)
      B$prev.LI <- ifelse(B$prev.LI<0,0,B$prev.LI)
      
      #------ Comparativo
      C <- data.frame(A,B,IND = A$indic + B$indic,N = 1:length(y))
      D <- subset(C,C$IND==2)
      if(nrow(D)>0)
      {
        for (j in 1:nrow(D))
        {
          if(!is.na(D$obs[j]))
          {
            #------ Previsao 1 passo a frente (fora da amostra)
            if(D$N[j] >= round(length(y)/2,0))
            {
              prevMLD <- as.numeric(na.exclude(MLD(Y = y[1:(D$N[j]-1)],period = 12,priori = FALSE,Fsaz = FALSE,logY = TRUE)$prevk1))
            } else {
              prevMLD <- as.numeric(na.exclude(MLD(Y = rev(y[(D$N[j]+1):length(y)]),period = 12,priori = TRUE,Fsaz = FALSE,logY = TRUE)$prevk1))
            }
            prevMLD <- max(0,prevMLD)
            
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
      
      #------ Atualizando valores
      GENERICA[,i+1] <- Aux.Y$valvalor
    }
    
    #------ Barra de progresso na tela
    utils::setTxtProgressBar(pb, i)
  }
  
  #------ Fechando conexao da barra de progresso
  close(pb)
  
  #------ Eliminando objetos 
  rm(A,B,C,D,Aux.Y,Aux.Y2,erro,i,j,opc,y,prevMLD,m,meses,sercod.tot)
  
  #------ Montando series nacionais ###
  
  ## CONFAZ12_ICMSN12
  GENERICA[,15] <- Matrix::rowSums(x = GENERICA[,c(2:14,16:29)],na.rm = TRUE)
  
  ## CONFAZ12_IPVA12
  GENERICA[,30] <- Matrix::rowSums(x = GENERICA[,31:57],na.rm = TRUE)
  
  ## CONFAZ12_ITCD12
  GENERICA[,58] <- Matrix::rowSums(x = GENERICA[,59:85],na.rm = TRUE)
  
  # GERANDO GRAFICOS --------------------------------------
  
  #------ Texto informativo
  message("Salvando graficos em //Srjn3/area_corporativa/Projeto_IPEADATA/Temporario/CONFAZgraf_verificacao")
  
  #------ Atualizacao da barra de progresso
  update.step <- max(5, floor(ncol(GENERICA)/100))
  
  #------ Barra de progresso
  pb <- utils::txtProgressBar(max = ncol(GENERICA), style = 3)
  
  for (j in 2:ncol(GENERICA))
  {
    if(substr(names(GENERICA)[j],10,13)=="IPVA")
    {
      #------ Salvando em formato .png
      grDevices::png(paste0("//Srjn3/area_corporativa/Projeto_IPEADATA/Temporario/CONFAZ12 - Gráficos de Verificação/",names(GENERICA)[j],".png"),width = 640, height = 480)
      graf.aux <- MLD(Y = GENERICA[,j],period = 12,dataY = GENERICA$valdata,plot = FALSE,priori = FALSE,IC = .9)
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
      legend("topleft",legend = c("Estimativa","Observação Corrigida","Observação Não Corrigida"),
             pch=c(NA,19,19),lwd=c(2,NA,NA),col=c(4,2,3),bty="n",horiz = T)
      dev.off() 
    } else {
      #------ Salvando em formato .png
      grDevices::png(paste0("//Srjn3/area_corporativa/Projeto_IPEADATA/Temporario/CONFAZ12 - Gráficos de Verificação/",names(GENERICA)[j],".png"),width = 640, height = 480)
      graf.aux <- MLD(Y = GENERICA[,j],period = 12,Fsaz = FALSE,plot = FALSE,priori = FALSE,IC = .9)
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
      legend("topleft",legend = c("Estimativa","Observação Corrigida","Observação Não Corrigida"),
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
  
  if(gerarGen)
  {
    # SALVANDO GENERICA --------------------------------------
    
    #------ Exportando xls
    xlsx::write.xlsx(x = GENERICA,
                     file = "//Srjn3/area_corporativa/Projeto_IPEADATA/ETL/Generica/GENERICA_CONFAZ12.xls",
                     sheetName="Generica", row.names=FALSE, showNA=FALSE)
    
    # ATUALIZANDO AUTOLOG --------------------------------------
    
    #------ Lendo autolog
    autolog <- read.csv2(file = "//Srjn3/area_corporativa/Projeto_IPEADATA/Temporario/PacoteIpeadataRio_AutoLog/autolog_pacoteipeadatario.csv")
    
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
               file = "//Srjn3/area_corporativa/Projeto_IPEADATA/Temporario/PacoteIpeadataRio_AutoLog/autolog_pacoteipeadatario.csv",
               row.names = FALSE)
    
    #------ Eliminando objetos 
    rm(autolog,r)
  }
  
  # TEXTO RESUMO ---------------------------------------- 
  
  cat("\n")
  cat("RESUMO \n")
  cat("Numero de Alterações \n")
  cat(paste("Total .................................",sum(GENERICA[,-1] == GENERICA.OLD[,-1],na.rm = TRUE)),"\n")
  cat("\n")
  
  #------ Resultado
  return(GENERICA)
}