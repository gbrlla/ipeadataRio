# --------------------------------------------------------- #
# |   INSTITUTO DE PESQUISA ECONOMICA APLICADA - IPEA     | #
# |                    PROJETO IPEADATA                   | #
# --------------------------------------------------------- #
# |   COORDENADOR: ERIVELTON P. GUEDES                    | #
# --------------------------------------------------------- #
# |   PROGRAMADOR: LUIZ EDUARDO S. GOMES                  | #
# --------------------------------------------------------- #
# |   MLD                                                 | #
# --------------------------------------------------------- #

# MLD -------------------------------------------------------

# -------------------------------------------------------------- #
# REFERENCIA:
# Multivariate DLM (Cap. 4)
# Bayesian forecasting and dynamic models (WEST & HARRISON, 1997)
# -------------------------------------------------------------- #

MLD <- function(Y,period,plot = FALSE,IC = .9,
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
  if(plot)
  {
    par(mar=c(4,4,.5,.5))
    plot(1,type="n",
         ylim=c(min(ft2.LI[,,-1]),max(ft2.LS[,,-1])),
         xlim=c(0,T),ylab="Série",
         xlab="Tempo")
    color_transparent <- adjustcolor("blue", alpha.f = 0.3)
    polygon(c(rev(1:T),1:T),c(rev(ft2.LS[,,-1]),ft2.LI[,,-1]),col=color_transparent,border=NA)
    lines(ft2[,,-1],col=4,lwd=2)
    points(Yt[,,-1],pch=19,cex=1,col=2)
    legend("topright",legend = c("Estimativa","Observação"),
           pch=c(NA,19),lwd=c(2,NA),col=c(4,2),bty="n",horiz = T)
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