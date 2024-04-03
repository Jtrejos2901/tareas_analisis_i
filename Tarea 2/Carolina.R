#------------------------Ejercicio 2--------------------------------------------
#a) Efectúe un ACP solo con las variables numéricas

library(FactoMineR)
library(factoextra)
library(ggplot2)
library(GGally)
library(corr)

set.seed(123)

beans_datos <- read.csv("Tarea 2/beansV2.csv")
str(beans_datos)


#ACP solo con las variables númericas
beans_ACP <- PCA(beans_datos[,-17], scale.unit=TRUE, ncp=5, graph = FALSE)
beans_ACP
beans_ACP$eig
beans_ACP$ind
beans_ACP$var


#b) Eliminar del plano principal los individuos mal representados
cos2.ind <-(beans_ACP$ind$cos2[,1]+beans_ACP$ind$cos2[,2])*100
cos2.ind

mi.tema <- theme_grey() + theme(panel.border = element_rect(fill = NA,color = "white"), plot.title = element_text(hjust = 0.5))

fviz_pca_ind(beans_ACP, col.ind = "#458B74",label = "none" , select.ind = list(cos2 = 0.1),ggtheme = mi.tema)


#Eliminar del círculo las variables mal representadas
cos2.var<-(beans_ACP$var$cos2[,1]+beans_ACP$var$cos2[,2])*100
cos2.var

fviz_pca_var(beans_ACP,col.var="#FF8C69", select.var = list(cos2 = 0.1),ggtheme = mi.tema)

#c) Encontrar 3 clústeres en el plano principal

# Filtrar los individuos con cos2 mayor a 0.1
inds_selected <- beans_ACP$ind$coord[rowSums(beans_ACP$ind$cos2[,1:2]) > 0.1, ]

# Realizar el clustering k-means sobre los individuos seleccionados
clusters <- kmeans(inds_selected[, 1:2], centers = 3)

# Graficar los clusters obtenidos
fviz_cluster(list(data = inds_selected, cluster = clusters$cluster), 
             geom = "point", 
             stand = FALSE,
             main = "PCA con Clusters")

#d) Otros gráficos de correlación

ggpairs(beans_datos[,-17],upper = list(continuous = wrap("cor", size = 2)))

correlaciones <- cor(beans_datos[,-17])
corrplot(correlaciones)


#e) Sobre posición gráficos 

fviz_pca_biplot(beans_ACP,col.var = "#FF8C69",col.ind = "#458B74",
                select.var = list(cos = 0.1), select.ind = list(cos = 0.1),
                geom.ind = "point",
                ggtheme = mi.tema)

#f) Convertir Class a código disyuntivo

#Se identifican las categorías de la variable class
categorias_class  <- unique(beans_datos$Class)
categorias_class
#Se hace código disyuntivo

lista_disyuntivo  <- list()
beans_datos2<- beans_datos[,-17]

for (i in 1:length(categorias_class)) {
  lista_disyuntivo[[i]] <- as.numeric(beans_datos$Class == categorias_class[i])
  names(lista_disyuntivo)[i] <- paste("Class_", categorias_class[i], sep = "")
  print(lista_disyuntivo[[i]])
  beans_datos2 <- cbind(beans_datos2, lista_disyuntivo[i])
}
str(beans_datos2)

#ACP
beans_ACP2 <- PCA(beans_datos2, scale.unit=TRUE, ncp=5, graph = FALSE)
beans_ACP2
beans_ACP2$eig
beans_ACP2$ind
beans_ACP2$var

#individuos y variables mal representados 
cos2.ind2<-(beans_ACP2$ind$cos2[,1]+beans_ACP2$ind$cos2[,2])*100
cos2.ind2

cos2.var2<-(beans_ACP2$var$cos2[,1]+beans_ACP2$var$cos2[,2])*100
cos2.var2

#gráficos de los individuos y variables con cos > 0.1
fviz_pca_ind(beans_ACP2, col.ind = "#458B74",label = "none" , select.ind = list(cos2 = 0.1),ggtheme = mi.tema)

fviz_pca_var(beans_ACP2,col.var="#FF8C69", select.var = list(cos2 = 0.1),ggtheme = mi.tema)


#Formación clústeres basado en la sobreposición de los gráficos 

# Filtrar los individuos con cos2 mayor a 0.1
inds_selected2 <- beans_ACP2$ind$coord[rowSums(beans_ACP2$ind$cos2[,1:2]) > 0.1, ]

# Realizar el clustering k-means sobre los individuos seleccionados
clusters2 <- kmeans(inds_selected2[, 1:2], centers = 3)

# Graficar los clusters obtenidos
fviz_cluster(list(data = inds_selected2, cluster = clusters2$cluster), 
             axes = c(1,2),
             geom = "point", 
             stand = FALSE,
             main = "PCA con Clusters")

# Sobre posición gráficos 

fviz_pca_biplot(beans_ACP2,col.var = "#FF8C69",col.ind = "#458B74",
                select.var = list(cos = 0.1), select.ind = list(cos = 0.1),
                geom.ind = "point",
                ggtheme = mi.tema)




