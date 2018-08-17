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

#' @export
#' 
#' @importFrom stats na.exclude
#' @importFrom utils setTxtProgressBar txtProgressBar

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
                prevMLD <- as.numeric(stats::na.exclude(MLD(Y = y[1:(D$N[j]-1)],period = 12,Fsaz = FALSE,priori = FALSE,logY = TRUE,IC = .9)$prevk1))
              } else {
                prevMLD <- as.numeric(stats::na.exclude(MLD(Y = rev(y[(D$N[j]+1):length(y)]),period = 12,Fsaz = FALSE,priori = TRUE,logY = TRUE,IC = .9)$prevk1))
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
                prevMLD <- as.numeric(stats::na.exclude(MLD(Y = y[1:(D$N[j]-1)],period = 12,dataY = Aux.Y$VALDATA,priori = FALSE,logY = TRUE,IC = .9)$prevk1))
              } else {
                prevMLD <- as.numeric(stats::na.exclude(MLD(Y = rev(y[(D$N[j]+1):length(y)]),period = 12,dataY = Aux.Y$VALDATA,priori = FALSE,logY = TRUE,IC = .9)$prevk1))
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
                prevMLD <- as.numeric(stats::na.exclude(MLD(Y = y[1:(D$N[j]-1)],period = 12,priori = FALSE,logY = TRUE,Fsaz = FALSE,IC = .9)$prevk1))
              } else {
                prevMLD <- as.numeric(stats::na.exclude(MLD(Y = rev(y[(D$N[j]+1):length(y)]),period = 12,priori = TRUE,logY = TRUE,Fsaz = FALSE,IC = .9)$prevk1))
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
    
  if(gerarGen)
  {
    # GERANDO GRAFICOS --------------------------------------
    
    #------ Texto informativo
    message(paste("Salvando graficos em",
                  file.path("","","Srjn3","area_corporativa","Projeto_IPEADATA","Geral","PacoteIpeadataRio","CONFAZ12 - Graficos de Verificacao")))
    
    #------ Atualizacao da barra de progresso
    update.step <- max(5, floor(ncol(GENERICA)/100))
    
    #------ Barra de progresso
    pb <- utils::txtProgressBar(max = ncol(GENERICA), style = 3)
    
    for (j in 2:ncol(GENERICA))
    {
      if(substr(names(GENERICA)[j],10,13)=="IPVA")
      {
        #------ Salvando em formato .png
        grDevices::png(filename = file.path("","","Srjn3","area_corporativa","Projeto_IPEADATA",
                                            "Geral","PacoteIpeadataRio","CONFAZ12 - Graficos de Verificacao",
                                            paste0(names(GENERICA)[j],".png")),width = 640, height = 480)
        graf.aux <- MLD(Y = GENERICA[,j],period = 12,dataY = GENERICA$VALDATA,priori = FALSE,IC = .9)
        graf.aux$prev <- ifelse(graf.aux$prev<0,0,graf.aux$prev)
        graf.aux$prev.LI <- ifelse(graf.aux$prev.LI<0,0,graf.aux$prev.LI)
        graphics::par(mar = c(4,4,.5,.5))
        graphics::plot(1,type="n",
                       ylim=c(max(0,min(graf.aux$prev.LI)),max(graf.aux$prev.LS)),
                       xlim=c(0,nrow(graf.aux)),ylab=names(GENERICA)[j],
                       xlab="Tempo")
        color_transparent <- grDevices::adjustcolor("blue", alpha.f = 0.3)
        graphics::polygon(c(rev(1:nrow(graf.aux)),1:nrow(graf.aux)),c(rev(graf.aux$prev.LS),graf.aux$prev.LI),col=color_transparent,border=NA)
        graphics::points(GENERICA.OLD[,j],pch=19,cex=1,col=3,t="o",lwd=3)
        graphics::points(graf.aux$obs,pch=19,cex=1,col=2,t="o",lwd=2)
        graphics::lines(graf.aux$prev,col=4,lwd=2)
        graphics::legend("topleft",legend = c("Estimativa","Observacao Corrigida","Observacao Nao Corrigida"),
                         pch=c(NA,19,19),lwd=c(2,NA,NA),col=c(4,2,3),bty="n",horiz = T)
        grDevices::dev.off() 
      } else {
        #------ Salvando em formato .png
        grDevices::png(filename = file.path("","","Srjn3","area_corporativa","Projeto_IPEADATA",
                                            "Geral","PacoteIpeadataRio","CONFAZ12 - Graficos de Verificacao",
                                            paste0(names(GENERICA)[j],".png")),width = 640, height = 480)
        graf.aux <- MLD(Y = GENERICA[,j],period = 12,Fsaz = FALSE,priori = FALSE,IC = .9)
        graf.aux$prev <- ifelse(graf.aux$prev<0,0,graf.aux$prev)
        graf.aux$prev.LI <- ifelse(graf.aux$prev.LI<0,0,graf.aux$prev.LI)
        graphics::par(mar = c(4,4,.5,.5))
        graphics::plot(1,type="n",
                       ylim=c(max(0,min(graf.aux$prev.LI)),max(graf.aux$prev.LS)),
                       xlim=c(0,nrow(graf.aux)),ylab=names(GENERICA)[j],
                       xlab="Tempo")
        color_transparent <- grDevices::adjustcolor("blue", alpha.f = 0.3)
        graphics::polygon(c(rev(1:nrow(graf.aux)),1:nrow(graf.aux)),c(rev(graf.aux$prev.LS),graf.aux$prev.LI),col=color_transparent,border=NA)
        graphics::points(GENERICA.OLD[,j],pch=19,cex=1,col=3,t="o",lwd=3)
        graphics::points(graf.aux$obs,pch=19,cex=1,col=2,t="o",lwd=2)
        graphics::lines(graf.aux$prev,col=4,lwd=2)
        graphics::legend("topleft",legend = c("Estimativa","Observacao Corrigida","Observacao Nao Corrigida"),
                          pch=c(NA,19,19),lwd=c(2,NA,NA),col=c(4,2,3),bty="n",horiz = T)
        grDevices::dev.off() 
      }
      #------ Barra de progresso na tela
      utils::setTxtProgressBar(pb, j)
    }
    
    #------ Fechando conexao da barra de progresso
    close(pb)
    
    #------ Eliminando objetos 
    rm(color_transparent,j,pb,update.step,graf.aux,sercod)
  }
  
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
    message(paste("Exportando planilha de atualizacao para",
                  file.path("","","Srjn3","area_corporativa","Projeto_IPEADATA","ETL","Generica")))
    
    # SALVANDO GENERICA --------------------------------------
    
    #------ Exportando xls
    xlsx::write.xlsx(x = GENERICA,
                     file = file.path("","","Srjn3","area_corporativa","Projeto_IPEADATA","ETL","Generica","GENERICA_CONFAZ12.xls"),
                     sheetName="Generica", row.names=FALSE, showNA=FALSE)
    
    # ATUALIZANDO AUTOLOG --------------------------------------
    
    #------ Lendo autolog
    autolog <- utils::read.csv2(file = file.path("","","Srjn3","area_corporativa","Projeto_IPEADATA","Geral","PacoteIpeadataRio","autolog.csv"))
    
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
    utils::write.csv2(x = autolog,
                      file = file.path("","","Srjn3","area_corporativa","Projeto_IPEADATA","Geral","PacoteIpeadataRio","autolog.csv"),
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