#--------------------------------Ejercicio 6------------------------------------

# b) Programe una función en R que calcula la distancia de Chebychev entre dos 
# vectores.

Chebychev <- function(vector1, vector2){
  restas <- numeric(length(vector1)-1)
  for (i in 1:length(vector1)) {
    restas[i] <- abs(vector1[i] - vector2[i]) 
  }
  restas <- unlist(restas)
  distancia <- max(restas)
  return(distancia)
}

# c) Programe una función en R que recibe un DataFrame calcula la matriz de 
# distancias usando la distancia de Chebychev entre dos vectores calculada 
# anteriormente.

matriz_distancias <- function(dataframe){
  n <- nrow(dataframe)
  p <- ncol(dataframe)
  matriz <- matrix(NA, nrow = n, ncol = n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      matriz[i,j] <- Chebychev(dataframe[i,2:p], dataframe[j,2:p])
    }
  }
  
  rownames(matriz) <- dataframe[,1]
  colnames(matriz) <- dataframe[,1]
  return(matriz)
}

# d) Para la tabla de datos EjemploAlgoritmosRecomendacion.csv ejecute un 
# Clustering Jerarquico de Chebychev y la agregacion Ward. Compare el resultado 
# con el obtenido en el ejercicio 1 usando distancia euclidea.

Datos6 <- read.csv("EjemploAlgoritmosRecomendacion.csv",header=TRUE, sep=";", 
                   dec=",")

distancias <- matriz_distancias(Datos6)
distancias <- as.dist(distancias)
clustering <- hclust(distancias, method = "ward.D")
plot(clustering)
