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

#f) Convertir Class a código disyuntivo

#Se identifican las categorías de la variable class
unique(beans_datos$Class)

#Se hace código disyuntivo

categorias_class  <- unique(beans_datos$Class)
lista_disyuntivo  <- list()
lista_nombres <- list()
beans_datos2<-beans_datos[,-17]

for( i in 1: length(categorias_class)) {
  lista_nombres[[i]] <-categorias_class[i]
  lista_disyuntivo[[i]] <- as.numeric(beans_datos$Class == categorias_class[i])
  lista_nombres$categorias_class[i] <- lista_disyuntivo[[i]] 
  print(lista_disyuntivo[[i]])
  beans_datos2 <-cbind(beans_datos2, lista_nombres[[i]])
}
str(beans_datos2)

#ACP
beans_ACP2 <- PCA(beans_datos2, scale.unit=TRUE, ncp=5, graph = FALSE)
beans_ACP2

#Eliminar del plano principal los individuos mal representados 
plot(beans_ACP2, axes=c(1, 2), choix="ind",col.ind="blue",new.plot=TRUE,select="cos2 0.1")
#Eliminar del círculo las variables mal representadas
plot(beans_ACP2, axes=c(1, 2), choix="var",col.var="green",new.plot=TRUE,select="cos2 0.1")

