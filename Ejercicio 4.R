#------------------------- Ejercicio 4-----------------------------------------

#librerías
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(GGally)

#Se carga la base de datos con una configuración de 4 decimales
concreto <- read_csv("concreto.csv")
concreto <- round(concreto, 4) 

#Análisis exploratorio de los datos 

str(concreto)
head(concreto)

#1.Verificar si hay outliers

#transformamos el dataframe para obtener uno con variables categóricas
stacked_concreto <- stack(concreto)
head(stacked_concreto)

#cajas de bigotes para cada variable
boxplot(stacked_concreto$values ~ stacked_concreto$ind,
        col = rainbow(ncol(concreto)), xlab = "Variables", ylab ="y")

#Se puede observar que la variable edad y agua son los que presenta más outiliers (3 y 4).
#También, los valores atípicos de las variables de edad y agregado fino se encuentran
#considerablemente lejanos al resto de los datos, por lo que se procede a eliminarse
#para que no se vea afectado el cálculo de estadísticos como la media y correlaciones.

#concreto$agregado_fino[concreto$agregado_fino == max(concreto$agregado_fino)]<-NA

#head(sort(concreto$edad, decreasing = TRUE),100)

#concreto$edad[concreto$edad ==  365 | concreto$edad ==  360 | concreto$edad ==  270 | concreto$edad ==  180]<-NA

#2.Usar summary
summary(concreto)

#3.Hacer gráficos de correlación
ggpairs(concreto,  upper = list(continuous = wrap("cor", size = 2.5)))
#A partir del gráfico se puede observar que la variable que mejor se correlaciona con
#resistencia_comprension es cemento.

#--------------------------Ejercicio 2------------------------------------------

analisis_col <- function(dataframe, col1, col2) {
  
  nombres_col <- colnames(dataframe[,c(col1,col2)])
  columna1 <- dataframe[, c(col1)]
  columna2 <- dataframe[, c(col2)]
  
  covarianza <- cov(columna1,columna2)
  correlacion <- cor(columna1,columna2)
  
  resultado <- paste0("Nombres columnas:", nombres_col[1], " y ", nombres_col[2]
                      ,"\nCovarianza:",covarianza,"\nCorrelación:", correlacion)
  
  return(cat(resultado))
}

analisis_col(concreto, 1, 2)

