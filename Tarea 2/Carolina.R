#------------------------Ejercicio 2--------------------------------------------

#a) Efecute un ACP solo con las variables numéricas

library(FactoMineR)
library(factoextra)

beans_datos <- read.csv("Tarea 2/beansV2.csv")
str(beans_datos)

#ACP solo con las variables númericas
beans_ACP <- PCA(beans_datos[,-17], scale.unit=TRUE, ncp=5, graph = FALSE)
beans_ACP

#b) Eliminar del plano principal los individuos mal representados 
plot(beans_ACP, axes=c(1, 2), choix="ind",col.ind="blue",new.plot=TRUE,select="cos2 0.1")
#Eliminar del círculo las variables mal representadas
plot(beans_ACP, axes=c(1, 2), choix="var",col.var="green",new.plot=TRUE,select="cos2 0.1")

