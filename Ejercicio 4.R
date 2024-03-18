#------------------------- Ejercicio 4-----------------------------------------

#librerías
library(readr)
library(dplyr)

#Se carga la base de datos con una configuración de 4 decimales
concreto <- read_csv("concreto.csv")
concreto <- rapply(object = concreto, f = round, classes = "numeric", 
                   how = "replace", digits = 4) 

#Análsis exploratorio de los datos 

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

concreto$agregado_fino[concreto$agregado_fino == max(concreto$agregado_fino)]<-NA

head(sort(concreto$edad, decreasing = TRUE),100)

concreto$edad[concreto$edad ==  365 | concreto$edad ==  360 | concreto$edad ==  270 
              | concreto$edad ==  180]<-NA

summary(concreto)

#2.Usar summary
#3.Hacer histograma para ver distribuciones
#4.Sacar correlaciones entre las variables y hacer gráficos de correlación