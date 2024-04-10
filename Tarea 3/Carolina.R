#---------------------------Ejercicio 2-----------------------------------------

library(FactoMineR)
library(readr)

#Para el desarrollo de los ejercicios se emplea la siguiente matriz
X <- matrix(c(8,1,0,4,6,5,6,8,7,10,4,7,8,2,5,0,3,6), nrow = 6, ncol = 3, byrow
            = TRUE)
X_inicial <- X

#Implemente en lenguaje R funciones que permitan ejecutar el algoritmo del Análisis
#en Componentes Principales (ACP) visto en clase para variables numéricas.
#Compare los resultados obtenidos con respecto a FactoMineR.

#El algoritmo del ACP cuenta con los siguientes pasos:

#1) Centrar y reducir la matriz 

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
C #Tiene las coordendas de los individuos 

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


#------------------ Ejercicio 8-------------------------------------------------
#Verifique todo lo programado en los puntos anteriores con el ejemplo estudiantes.csv
# y con los datos del ejercicio 1 de la tarea anterior

#----- Estudiantes--------

estudiantes_datos <- read.table('Tarea 3/EjemploEstudiantes.csv', header=TRUE, sep=';',dec=',',row.names=1)
estudiantes_datos<- as.matrix(estudiantes_datos)
estudiantes_datos_original <- estudiantes_datos

#1) Centrar y reducir la matriz 

medias_estudiantes <- medias(estudiantes_datos)
medias_estudiantes

sd_estudiantes <- sd_poblacional(estudiantes_datos)
sd_estudiantes

estudiantes_datos <- centrar_y_reducir(estudiantes_datos,medias_estudiantes, sd_estudiantes)
estudiantes_datos


#2)Calcular la matriz de correlaciones 

estudiantes_R <- R(estudiantes_datos)
estudiantes_R


#3 y 4) Calcular los vectores y valores propios de la matriz de correlaciones y 
#ordenar los valores propios de mayor a menor

estudiantes.R_e <- eigen(estudiantes_R)

estudiantes.R_valores.propios <- estudiantes.R_e$values #ya vienen ordenados de mayor a menor
estudiantes.R_valores.propios

estudiantes.R_vectores.propios <- estudiantes.R_e$vectors
estudiantes.R_vectores.propios

#5) Construir matriz V que tiene como columnas los vectores propios de la matriz
#de correlaciones

V_estudiantes <- estudiantes.R_vectores.propios
V_estudiantes

#6) Calcular la matriz de componentes principales

C_estudiantes <- estudiantes_datos%*%V_estudiantes
C_estudiantes

#7) Calcular la matriz de calidades de los individuos (cosenos cuadrados)

estudiantes_Q <- Q(C_estudiantes, estudiantes_datos)
estudiantes_Q

#Ahora debemos calcular la contribución de cada individuo i a la varianza total 
#del eje r

estudiantes_contrib <- contribucion(C_estudiantes, estudiantes.R_valores.propios)
estudiantes_contrib

#8) Calcular la matriz de coordenadas T de las variables

estudiantes_T <- T(V_estudiantes, estudiantes.R_valores.propios)
estudiantes_T

#9)Calcular la matriz de calidades de las variables (cosenos cuadrados)

S_estudiantes <- estudiantes_T^2
S_estudiantes

#10) Calcular vector I (1xm) de inercias de los ejes

estudiantes_I <- I(estudiantes.R_valores.propios)
estudiantes_I

ACP <-PCA(estudiantes_datos)
plot(ACP)

#----- beans--------

beans_datos <- read.csv("Tarea 3/beansV2.csv")
beans_datos <- as.matrix(beans_datos[,-17])
beans_datos_original <- beans_datos

#1) Centrar y reducir la matriz 

medias_beans <- medias(beans_datos)
medias_beans

sd_beans <- sd_poblacional(beans_datos)
sd_beans

beans_datos <- centrar_y_reducir(beans_datos,medias_beans, sd_beans)
beans_datos


#2)Calcular la matriz de correlaciones 

beans_R <- R(beans_datos)
beans_R


#3 y 4) Calcular los vectores y valores propios de la matriz de correlaciones y 
#ordenar los valores propios de mayor a menor

beans.R_e <- eigen(beans_R)

beans.R_valores.propios <- beans.R_e$values #ya vienen ordenados de mayor a menor
beans.R_valores.propios

beans.R_vectores.propios <- beans.R_e$vectors
beans.R_vectores.propios

#5) Construir matriz V que tiene como columnas los vectores propios de la matriz
#de correlaciones

V_beans <- beans.R_vectores.propios
V_beans

#6) Calcular la matriz de componentes principales

C_beans <- beans_datos%*%V_beans
C_beans

#7) Calcular la matriz de calidades de los individuos (cosenos cuadrados)

beans_Q <- Q(C_beans, beans_datos)
beans_Q

#Ahora debemos calcular la contribución de cada individuo i a la varianza total 
#del eje r

beans_contrib <- contribucion(C_beans, beans.R_valores.propios)
beans_contrib

#8) Calcular la matriz de coordenadas T de las variables

beans_T <- T(V_beans, beans.R_valores.propios)
beans_T

#9)Calcular la matriz de calidades de las variables (cosenos cuadrados)

S_beans <- beans_T^2
S_beans

#10) Calcular vector I (1xm) de inercias de los ejes

beans_I <- I(beans.R_valores.propios)
beans_I

ACP_beans <-PCA(beans_datos)
plot(ACP_beans)


#----------------- Ejercicio 5--------------------------------------------------

#5. Programe una función en R que reciba una columna (variable) de una matriz y
#calcule su proyección en suplementario en el círculo de correlaciones 2D 
#programado en el punto2. Compare los resultados obtenidos con respecto a 
#FactoMineR


var.sup_proyeccion <- function(columna, matriz) {
  #se calcula la media y desviación estándar de la columna y la matriz
  media <- mean(columna)
  n <- length(columna)
  sd <-sqrt(((n-1)/n))*sd(columna)
  
  medias <- medias(matriz)
  sd_matriz <-sd_poblacional(matriz)
  
  #centramos y reducimos la columna y la matriz
  
  columna <- (columna-media)/sd
  matriz <- centrar_y_reducir(matriz, medias, sd_matriz)
  
  #Matriz de correlaciones
  correlaciones <- R(matriz)
  
  #Vectores y valores propios
  matriz.e <- eigen(correlaciones)
  V <- matriz.e$vectors

  #Se calculan los componentes principales
  C <- matriz%*%V
  
  #Se calculan las correlaciones de la columna con los componentes principales
  coordenada <- c()
  for(i in 1: ncol(matriz)){
    coordenada[i] <-cor(columna, C[,i])
  }
  #Se gráfica en el círculo de correlaciones
  circulo <- circulo_correlaciones(matriz)
  graf_circulo <- circulo$variables
  
  #Convertimos la matriz en un dataframe y ajustamos para el gráfico
  coordenada_var <- as.data.frame(t(coordenada))
  col_names <- paste("Dim", 1:ncol(coordenada_var))  # Genera nombres como "Dim 1", "Dim 2", etc.
  colnames(coordenada_var) <- col_names
  coordenada_var$`x origen` <- 0
  coordenada_var$`y origen` <- 0
  
  if(is.null(colnames(columna))){
   coordenada_var$variable <- ncol(matriz) + 1
  }else {
    coordenada_var$variable <- colnames(columna)
  }
  
  resultado <- graf_circulo + 
    geom_segment(data = coordenada_var, aes(x = `x origen`, y = `y origen`, xend = `Dim 1`, yend = `Dim 2`), 
    arrow = arrow(length = unit(0.2, "inches")), color = "red")+
    geom_text(data = coordenada_var, aes(x = `Dim 1`, y = `Dim 2`), 
              vjust = -0.5, hjust = -0.5, color = "red", label = coordenada_var$variable )
  return(resultado)
}

columna <- X_inicial[,3]
coordenadas_sup <- var.sup_proyeccion(columna, X_inicial[,-3])
coordenadas_sup

#Comparando con FactoMiner
ACP <- PCA( X_inicial,quanti.sup = 3, graph = F)
plot.PCA(ACP, choix = "var")

#Comparando, quedan igual solo que reflejadas con respecto al eje X.


#----------------------------Ejercicio 8---------------------------------------

#Estudiantes
columna_sup_estudiantes <- as.matrix(estudiantes_datos_original[,2])
colnames(columna_sup_estudiantes) <- (colnames(estudiantes_datos_original))[2]

coordenadasvar_sup_estudiantes <- var.sup_proyeccion(columna_sup_estudiantes, estudiantes_datos_original[,-2])
coordenadasvar_sup_estudiantes

#beans
columna_sup_beans <- as.matrix(beans_datos_original[,2])
colnames(columna_sup_beans) <- (colnames(beans_datos_original))[2]

coordenadasvar_sup_beans <- var.sup_proyeccion(columna_sup_beans, beans_datos_original[,-2])
coordenadasvar_sup_beans
