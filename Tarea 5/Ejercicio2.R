#-------------------------EJERCICIO 2-------------------------------------------

# 2. Programe el algoritmo para Escalamiento Multidimensional (Mutidimensional
# Scaling) visto en clase y con la tabla de estudiantes compare con cmdscale(...)
# de R y verifique los resultados.

#Se cargan las librerías necesarias
library(FactoMineR)
library(factoextra)

# A continuación, se presenta una función que realiza el MDS paso a paso

MDS_algoritmo <- function(X, distancias) {
  
  # Paso 1: Con las distancias, se obtiene la matriz B 
  n <- nrow(X) # cantidad de individuos
  B_matriz <- matrix(NA, n, n)
  suma3 <- sum(distancias^2)
  
  for (r in 1: n) {
    suma1 <- 0
    suma2 <- 0
    for (s in 1: n ) {
      suma1 <- sum((distancias[,s])^2)
      suma2 <- sum((distancias[r,])^2)
      B_matriz[r,s] <- -(1/2)*(distancias[r,s]^2 - (1/n)*(suma1 + suma2) 
                               + (1/n^2)*suma3) 
    }
  }
  
  # Paso 2 : Se calculan los valores y vectores propios de B.
  B_e <- eigen(B_matriz)
  valores_propios <- B_e$values
  
  # Los valores propios muy pequeños cercanos a cero, se ignoran para el cálculo 
  # de las coordenadas. Se define un umbral para los valores propios 
  umbral <- 1e-10 
  
  # Se filtran los valores propios mayores que el umbral
  valores_propios_filtrados <- valores_propios[valores_propios > umbral]
  # Se obtienen los vectores propios asociados a los valores propios filtrados
  m <- length(valores_propios_filtrados)
  vectores_propios <- B_e$vectors[, 1:m]
  
  # Paso 3 : Se calculan las coordenadas
  coordenadas <- matrix(NA, n, m)
  rownames(coordenadas) <- rownames(X)
  
  for(i in 1: n) {
    for(j in 1: m){
      coordenadas[i,j] <- (sqrt(valores_propios_filtrados[j])*
                             vectores_propios[i,j])
    }
  }
  
  # Paso 4: Graficar 
  x <- coordenadas[,1]
  y <- coordenadas[,2]
  
  grafico <- plot(x, y, xlab="Componente 1", ylab="Componente 2",
                  main="MDS algoritmo",pch = 19)
  text(x, y, labels = row.names(X), cex = 0.85 , pos = 1)
  
  resultado <- list("points" = coordenadas, "eig" = valores_propios, 
                    "plot"= grafico)
  
  return(resultado)
}

# Comparación con cmdscales() 

# Primeramente, se carga la base de datos de estudiantes
estudiantes_datos <- read.table('Tarea 3/EjemploEstudiantes.csv', 
                                header=TRUE, sep=';',dec=',',row.names=1)

# Se obtiene las distancias de cada par de individuos
distancias <- dist(estudiantes_datos)
distancias_matriz <- as.matrix(distancias)

# Se verifican los resultados obtenidos por ambas funciones
MDS_estudiantes <- cmdscale(distancias, k = 5, eig=TRUE)
MDS_estudiantes

x <- MDS_estudiantes$points[,1]
y <- MDS_estudiantes$points[,2]

plot(x, y, xlab="Componente 1", ylab="Componente 2",
                main="MDS",pch = 19)
text(x, y, labels = row.names(estudiantes_datos), cex = 0.85 , pos = 1)

MDS_algoritmo_estudiantes <- MDS_algoritmo(estudiantes_datos, distancias_matriz)

# Las coordenadas de los individuos son iguales. Por otro lado, los primeros 5
# valores propios dados por ambos son los mismos. En cuanto a los restantes 4,
# estos difieren,pues, al ser números cercanos a cero por cuestiones de 
# de cálculo de la función cmdscale la precisión númerica cambia en comparación 
# con eigen.
# Por último, el plano principal es el mismo.
