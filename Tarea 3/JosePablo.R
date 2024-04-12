library(FactoMineR)
library(readr)

# Ejercicio 6

# Primero se genera la matriz
X <- matrix(c(8,1,0,4,6,5,6,8,7,10,4,7,8,2,5,0,3,6), nrow = 6, ncol = 3, 
            byrow = TRUE)

# Se crea una función para centrar y reducir la matriz inicial.
centrar_y_reducir <- function(matriz){
  # Se obtiene la cantidad de filas y columnas que posee la matriz.
  columnas <- dim(matriz)[[2]]
  filas <- dim(matriz)[[1]]
  
  # Se crea una nueva matriz con los datos centrados y reducidos.
  for(i in 1:columnas){
    matriz[,i] <- (matriz[,i] - mean(matriz[,i])) / (sd(matriz[,i])*sqrt((filas - 1)/filas))
  }
  
  return(matriz)
}

# Se calcula la nueva matriz centrada y reducida.
X.cyr <- centrar_y_reducir(X)

# Se crea una función para generar la matriz H.
calcular_H <- function(matriz){
  # Se retorna la matriz de Varianzas-covarianzas.
  return((1/dim(matriz)[[1]])* matriz %*% t(matriz))
}

# Se corre la función y se genera la matriz H.
H <- calcular_H(X.cyr)
H 

# Se calculan los valores y vectores propios de la matriz H.
e <- eigen(H)
val.propios <- e$values
vec.propios <- e$vectors

val.propios
vec.propios

# Se crea una función que calcule las coordenadas de las variables.
calcular_coordenadas <- function(matriz, vectores.propios){
  # Se obtiene la cantidad de filas y columnas que posee la matriz.
  columnas <- dim(matriz)[[2]]
  
  # Se crea una matriz en blanco la cual se rellenará con las coordenadas.
  coordenadas <- matrix(nrow = columnas, ncol = columnas)
  
  # Se rellena la matriz mediante un resultado obtenido a partir de la relación
  # de dualidad.
  for(i in 1:columnas){
    for(j in 1:columnas){
      x <- X.cyr[,i]
      v <- vec.propios[,j]
      
      coordenadas[i,j] <- t(x)%*%v/sqrt(t(x)%*%x%*%t(v)%*%v)
    }
  }
  
  return(coordenadas)
}

# Se calcula la matriz de coordenadas para las variables y se compara con las 
# obtenidas por FactoMineR.
coordenadas.var <- calcular_coordenadas(X.cyr, vec.propios)
ACP <- PCA(X.cyr, graph = FALSE)

coordenadas.var
ACP$var$coord

# Una vez que se cuenta con matriz de coordenadas, se procede a elevar al
# cuadrado dicha matriz para obtener la matriz de cosenos cuadrados. Además se
# compara el resultado con el obtenido con FactoMineR.
cos2.var <- (coordenadas.var)^2

cos2.var
ACP$var$cos2


ACP$var$contrib

Porc.contribucion<-matrix(NA,3,3)
for(i in 1:3){
  for(r in 1:3){
    Porc.contribucion[i,r]=(coordenadas.var[i,r]^2)/(3*val.propios[r])
  }
}

Porc.contribucion*100

ACP$ind$coord
ACP$ind$cos2
ACP$ind$contrib


