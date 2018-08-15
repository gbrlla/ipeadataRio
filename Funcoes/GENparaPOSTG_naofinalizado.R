GENparaPOSTG <- function(generica)
{
  #------ Inicializando DF
  GERADO <- data.frame(NULL)
  
  #------ Lista de variaveis
  sercod <- unique(generica[,1])
  
  for(i in 1:length(sercod))
  {
    aux <- data.frame(generica[,1],generica[,which(names(generica)==sercod[i])])
    
    
    GERADO <- rbind(GERADO,data.frame(serid=as.integer(NA),
                                      valdata=as.Date(aux[,1], origin = "1900-01-01"),
                                      terid=as.integer(1),
                                      valvalor=aux[,l],
                                      ocrid=as.integer(NA),
                                      sercodigotroll=names(aux)[l],
                                      atualizdata=Sys.time(),row.names = NULL))
    
  }
  

  
  
}