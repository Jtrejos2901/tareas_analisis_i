#------------------------- Ejercicio 4-----------------------------------------
#librerías
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(GGally)
library(univariateML)
library(rriskDistributions)
library(fitdistrplus)

#Se carga la base de datos con una configuración de 4 decimales 
concreto <- read_csv("concreto.csv")

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

#Se puede observar que la variable edad y agua son los que presentan más outiliers (3 y 4).
#También, ciertos valores atípicos de la variable de edad  se encuentran
#considerablemente lejanos al resto de los datos, sin embargo, no se proceden a eliminar 
#pues, el outlier más alto es 365 días  lo cuál, considerando
#la definición de esta variable y  las propiedades del concreto es un valor que
#se debe tomar en cuenta. En el caso de las otras variables que poseen outliers,
#estos se encuentra no muy lejos de los límites de la caja, y otros están muy
#cercanos al límite superior o inferior como es el caso de agua;por ende, tampoco
#se eliminan pues, no se consideran datos muy lejanos del resto o que no tienen
#sentido y no representan un problema para el cálculo de ciertos estadísticos 
#como la media o correlaciones.


#2.Usar summary
summary(concreto)

#3.Vamos a analizar qué tipo de densidad paramétrica se aproxima mejor a la
#variable resistencia_comprension mediante el criterio AIC

#método 1
model_select(concreto$resistencia_compresion, models = univariateML_models, criterion = "aic",
             na.rm = FALSE)
# Según el resultado, la densidad que mejor se ajusta es la Weibull

#método 2

fit.cont(concreto$resistencia_compresion)

#Mediante el criterio AIC, la densidad Weibull también es la que mejor se ajusta
#a los datos de resistencia_comprension

#En el siguiente gráfico se puede observar cómo es el ajuste.
fw <- fitdist(concreto$resistencia_compresion, "weibull")

par(mfrow = c(2,2))

denscomp(fw, legendtext = "Weibull")
qqcomp(fw, legendtext = "Weibull")
cdfcomp(fw, legendtext = "Weibull")
ppcomp(fw, legendtext = "Weibull")

#4.Hacer gráfico de correlación
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
  
  resultado <- paste0("Nombres columnas: ", nombres_col[1], " y ", nombres_col[2]
                      ,"\nCovarianza: ",covarianza,"\nCorrelación: ", correlacion)
  
  return(cat(resultado))
}

analisis_col(concreto, 7, 9)

#------------------------------Ejercicio 3--------------------------------------

#Función recursiva

U_n <- function(n) {
  if (n == 0) {
    U <- 5
  } else if (n == 1) {
    U <- -5
  } else if (n == 2) {
    U <- 2
  } else {
    U <- 4 * U_n(n - 1) - 15 * U_n(n - 2) + U_n(n - 3)
  }
  return(U) 
}

U_n(3)
