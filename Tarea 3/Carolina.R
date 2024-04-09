#---------------------------Ejercicio 2-----------------------------------------

library(FactoMineR)

#Para el desarrollo de los ejercicios se emplea la siguiente matriz
X <- matrix(c(8,1,0,4,6,5,6,8,7,10,4,7,8,2,5,0,3,6), nrow = 6, ncol = 3, byrow
            = TRUE)
X_inicial <- X

#Implemente en lenguaje R funciones que permitan ejecutar el algoritmo del Análisis
#en Componentes Principales (ACP) visto en clase para variables numéricas.
#Compare los resultados obtenidos con respecto a FactoMineR.

#El algoritmo del ACP cuenta con los siguientes pasos:

#1) Centrar y reducir la matriz 
#Para esto le restamos la media y dividimos por la desviación estándar a cada 
#columna de la matriz. Por tanto, primero calculamos las medias y desviaciones 
#estándar correspondientes con las siguiente funciones

#Medias

medias <- function(matriz) {
  resultado <-list()
  for(i in 1:ncol(matriz)) {
    resultado[[i]] <- mean(matriz[,i])
  }
  return(resultado)
}

#desviaciones estándar poblacionales 

sd_poblacional <- function(matriz) {
  resultado <- list()
  n <- nrow(matriz)
  for(i in 1:ncol(matriz)) {
    resultado[[i]] <- sqrt(((n-1)/n))*sd(matriz[,i])
  }
  return(resultado)
}
  
#Centramos y reducimos con la siguiente función

centrar_y_reducir <- function(matriz, medias, desviaciones) {
  
  for(i in 1 : ncol(matriz)){
    matriz[, i] <- (matriz[,i]-medias[[i]])/ desviaciones[[i]]
  }
  return(matriz)
}


#Aplicamos las funciones anteriores a la matriz X
medias_X <- medias(X)
medias_X

sd_X <- sd_poblacional(X)
sd_X

X <- centrar_y_reducir(X,medias_X, sd_X)
X


#2)Calcular la matriz de correlaciones 

#Una vez centrada y reducida la matriz, se procede a calcular la matriz de 
#varianzas-covarianzas la cual, es la misma que la matriz de correlaciones pues 
#está centrada y reducida. Se calcula con la siguiente función

R <- function(matriz) {
  n <- nrow(matriz) 
  resultado <- (1/n)*t(matriz)%*%matriz
  return(resultado)
}

#Obtenemos mediante esa función la matriz de correlaciones de X

X_R <- R(X)
X_R

#3 y 4) Calcular los vectores y valores propios de la matriz de correlaciones y 
#ordenar los valores propios de mayor a menor

X.R_e <- eigen(X_R)

X.R_valores.propios <- X.R_e$values #ya vienen ordenados de mayor a menor
X.R_valores.propios

X.R_vectores.propios <- X.R_e$vectors
X.R_vectores.propios

#Comparamos con lo obtenido con FactoMiner

X_ACP <- PCA(X, ncp = 4, graph = FALSE)
X_ACP$eig

#Se puede observar que los valores propios obtenidos con FactoMiner son iguales
#a los del algoritmo

#5) Construir matriz V que tiene como columnas los vectores propios de la matriz
#de correlaciones

V <- X.R_vectores.propios
V

#6) Calcular la matriz de componentes principales

C <- X%*%V
C

#Con FactoMiner se obtiene lo siguiente:
X_ACP$ind$coord


#Podemos observar que lo único que varía es el signo de algunas entradas, pero
#esto solo indica que se refleja con respecto a algunos de los ejes al graficar.

#7) Calcular la matriz de calidades de los individuos (cosenos cuadrados)

Q <- function(C, matriz) {
  n <-nrow(matriz)
  m <-ncol(matriz)
  resultado <- matrix(0, n,m)
  
  for(i in 1: n){
    suma <- 0 
    for(j in 1: m){
      suma <- suma + matriz[i,j]^2
    }
    for(r in 1: m){
      resultado[i,r] <- (C[i,r]^2)/suma
    }
  }
  return(resultado)
}

#Aplicamos la función anterior a los datos que tenemos

X_Q <- Q(C, X)
X_Q

#Vemos los resultados de FactoMiner
X_ACP$ind$cos2

#Los cuales son iguales a los obtenidos con el algoritmo

#Ahora debemos calcular la contribución de cada individuo i a la varianza total 
#del eje r

contribucion <- function(C, valores.propios) {
  n <-nrow(C)
  m <-ncol(C)
  resultado <- matrix(0, n, m)
  
  for(i in 1: n){
    for(r in 1: m){
      resultado[i,r] <- ((C[i,r]^2)/(n*valores.propios[r]))*100
    }
  }
  return(resultado)
}

#La matriz de contribuciones es:

X_contrib <- contribucion(C, X.R_valores.propios)
X_contrib

#Con FactoMiner se tiene:
X_ACP$ind$contrib

#Por lo tanto, se tiene el mismo resultado

#8) Calcular la matriz de coordenadas T de las variables

T <- function(V, valores.propios){
  m <- ncol(V)
  resultado <- matrix(0, m, m)
  
  for(r in 1:m){
      resultado[,r] <- sqrt(valores.propios[r])*V[,r]
    }
    
  return(resultado)
}

#La matriz de coordenadas de las variables para los datos que tenemos es:
X_T <- T(V, X.R_valores.propios)
X_T

#Y la dada con FactoMiner es:
X_ACP$var$coord

#9)Calcular la matriz de calidades de las variables (cosenos cuadrados)

S <- X_T^2
S

#Con FactoMiner da:
X_ACP$var$cos2

#10) Calcular vector I (1xm) de inercias de los ejes

I <- function(valores.propios){
  m <- length(valores.propios)
  resultado <- c()
  
  for(j in 1: m){
    resultado[j] <-100*(valores.propios[j]/m)
  }
  return(resultado)
}

#El vector de inercias para los datos con los que estamos trabajando es:
X_I <- I(X.R_valores.propios)
X_I

#Con FactoMiner se tiene:
plot(X_ACP, axes=c(1, 2), choix="ind", col.ind="blue",new.plot=TRUE)


#Podemos observar que las inercias correpondiente al eje x y y obtenidas con 
#FactoMiner son iguales a las dadas por el algoritmo.