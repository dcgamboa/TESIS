#.............................................................................
#
# silhouetteNClus.R
# 
# Fecha de Creaci�n : 19-04-2018
# 
# Autor:              Carolina Gamboa. 
# 
# Descripci�n:  Esta funci�n calcula el indice de silueta para determinadas 
#               cantidades de grupos, y determina la cantidad de grupos optima
#               usando el estad�stico de silueta. 
# 
# Entradas: Tres par�metros. K : Cantidad m�xima de grupos a analizar
#                            dis : Matriz de distancias. 
#                            method: M�todo Jer�rquico "single", "average"  
# 
# Output: Dos par�metros. K : Cantidad de grupos
#                         list : valor del �ndice de silueta para cada k 
# 
#.............................................................................



silhouetteNClus <- function(K, dis, method){
  
  silIndex <- data.frame(k = 1, silIndex = 0)
  
  if(method == "complete"){
 
      
      Agnes.dist <- agnes(dis, method="complete")
      Cl.Agnes <- cutree(Agnes.dist, 2:K)
      
      for (jj in 2:(K)) {
      coef <- silhouette(Cl.Agnes[, jj-1], dis)
      jjSilIndex <- mean(coef[, "sil_width"])
      silIndex <- rbind(silIndex, data.frame(k = jj, silIndex = jjSilIndex))
      
    }
    
    
  }

  if(method == "average"){
    
    
    Agnes.dist <- agnes(dis, method="average")
    Cl.Agnes <- cutree(Agnes.dist, 2:K)
    
    for (jj in 2:(K)) {
      coef <- silhouette(Cl.Agnes[, jj-1], dis)
      jjSilIndex <- mean(coef[, "sil_width"])
      silIndex <- rbind(silIndex, data.frame(k = jj, silIndex = jjSilIndex))
      
    }
    
    
  }
  
  
  
  if(method == "single"){
    
    
    Agnes.dist <- agnes(dis, method="single")
    Cl.Agnes <- cutree(Agnes.dist, 2:K)
    
    for (jj in 2:(K)) {
      coef <- silhouette(Cl.Agnes[, jj-1], dis)
      jjSilIndex <- mean(coef[, "sil_width"])
      silIndex <- rbind(silIndex, data.frame(k = jj, silIndex = jjSilIndex))
      
    }
    
    
  }
  maxPos <- which(silIndex[, "silIndex"]==max(silIndex[, "silIndex"]))
  cluster <- list(K = silIndex[maxPos, "k"], coef = silIndex)
  return(maxPos)

  }
  