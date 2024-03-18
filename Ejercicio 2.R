#Desarrolle una función R que recibe un DataFrame y dos números de columna y 
#que retorna el nombre de las variables correspondientes a las columnas,la 
#covarianza y la correlación entre esas dos variables.

escanear <- function(dataframe, columna1, columna2){
  nombre_variables <- vector(length = 2) 
  nombre_variables[1] <- colnames(dataframe)[columna1]
  nombre_variables[2] <- colnames(dataframe)[columna2]
  covarianza <- cov(dataframe[, columna1], dataframe[, columna2])
  correlacion <- cor(dataframe[, columna1], dataframe[, columna2])
  return(list(nombre_variables = nombre_variables, covarianza = covarianza, 
              correlación = correlacion))
}

#Prueba

prueba <- read.csv("concreto.csv")

escanear(prueba, 3, 5)