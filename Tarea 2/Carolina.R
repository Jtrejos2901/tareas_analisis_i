#------------------------Ejercicio 1--------------------------------------------
#a) Efectúe un ACP solo con las variables numéricas

library(FactoMineR)
library(factoextra)
library(ggplot2)
library(GGally)
library(corrplot)
library(plotly)

set.seed(123)
mi.tema <- theme_grey() + theme(panel.border = element_rect(fill = NA,color = "white"), plot.title = element_text(hjust = 0.5))

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
                select_var = list(cos2 = 0.1), select.ind =list(cos2 = 0.1),
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

# Filtrar los individuos  con cos2 mayor a 0.1
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
                select.var = list(cos2 = 0.1), select.ind = list(cos2 = 0.1),
                geom.ind = "point",
                ggtheme = mi.tema)


#------------------------Ejercicio 2--------------------------------------------

water_datos <- read.csv("Tarea 2/water_potability.csv")
str(water_datos)

#Transformar la variable Potability en categórica

for(i in 1: length(water_datos$Potability)){
  if(water_datos$Potability[i] == 0){
      water_datos$Potability[i] <- "No"
  }else
    water_datos$Potability[i] <- "Si"
}

#b) 

#ACP solo con las variables númericas
water_ACP <- PCA(water_datos[,-10], scale.unit=TRUE, ncp=5, graph = FALSE)
water_ACP
water_ACP$eig
water_ACP$ind
water_ACP$var

#1) elimine individuos mal representados y variables mal representadas 
#(coseno cuadrado menor al 5 %)

cos2.ind_water <-(water_ACP$ind$cos2[,1]+water_ACP$ind$cos2[,2])*100
cos2.ind_water

cos2.var_water<-(water_ACP$var$cos2[,1]+water_ACP$var$cos2[,2])*100
cos2.var_water

plano_inicial <-fviz_pca_ind(water_ACP, col.ind = "#87CEFA",label = "none" , select.ind = list(cos2 = 0.05),ggtheme = mi.tema)
plano_inicial

fviz_pca_var(water_ACP,col.var="#CD4F39", select.var = list(cos2 = 0.05),ggtheme = mi.tema)

#2) en el plano principal identifique un cluster en cada cuadrante

# Filtrar los individuos con cos2 mayor a 0.05
inds_selected_water <- water_ACP$ind$coord[rowSums(water_ACP$ind$cos2[,1:2]) > 0.05, ]
inds_selected_water <- as.data.frame(inds_selected_water)

#Se definen límites para los cuadrantes
x_median <- median(inds_selected_water$Dim.1)
y_median <- median(inds_selected_water$Dim.2)

# Crear una variable para almacenar los clusters correspondientes a cada punto
inds_selected_water$cluster <- ifelse(inds_selected_water$Dim.1 > x_median,
                                      ifelse(inds_selected_water$Dim.2 > y_median, 1, 2),
                                      ifelse(inds_selected_water$Dim.2 > y_median, 4, 3))

# Graficar los puntos con colores según los clusters en el plano inicial
plano_inicial + geom_point(data = inds_selected_water, aes(x = Dim.1, y = Dim.2,
  color = factor(cluster)), size = 3)+
  labs(color = "cluster")

#4) explique la formación de los clusteres basado en la sobre-posicion del circulo y el plano.

fviz_pca_biplot(water_ACP,col.var = "#CD4F39",col.ind = "#87CEFA",
                select.var = list(cos2 = 0.05), select.ind = list(cos2 = 0.05),
                geom.ind = "point",
                ggtheme = mi.tema)

#c) En el circulo de correlacion, usando los componentes 1 y 3, interprete la correlacion entre
#las variables Conductivity, Trihalomethanes y Organic carbon, que estan mal representadas
#en los componentes 1 y 2.

cos2.var_water2<-(water_ACP$var$cos2[,1]+water_ACP$var$cos2[,3])*100
cos2.var_water2

fviz_pca_var(water_ACP, axes = c(1,3), col.var="#CD4F39", select.var = list(cos2 = 0.05),ggtheme = mi.tema)

#d) convierta la variable Potability en Codigo Disyuntivo Completo y repita
#el ACP

#Se hace código disyuntivo

Potability_No <- as.numeric(water_datos$Potability == "No")
Potability_Si <- as.numeric(water_datos$Potability == "Si")

print(Potability_No)
print(Potability_Si)

water_datos2<- water_datos[,-10]
water_datos2 <- cbind(water_datos2, Potability_No)
water_datos2 <- cbind(water_datos2, Potability_Si)
str(water_datos2)

#ACP
water_ACP2 <- PCA(water_datos2, scale.unit=TRUE, ncp=5, graph = FALSE)
water_ACP2
water_ACP2$eig
water_ACP2$ind
water_ACP2$var

#Ver individuos y variables mal representados 
cos2.ind_3dim<-(water_ACP2$ind$cos2[,1]+water_ACP2$ind$cos2[,2]+ water_ACP2$ind$cos2[,3])*100
cos2.ind_3dim

cos2.var_3dim<-(water_ACP2$var$cos2[,1]+water_ACP2$var$cos2[,2] +water_ACP2$var$cos2[,3])*100
cos2.var_3dim

# Filtrar los individuos y variables con cos2 mayor a 0.05
inds_selected_water2 <- water_ACP2$ind$coord[rowSums(water_ACP2$ind$cos2[,1:3]) > 0.05, ]
vars_selected_water2 <- water_ACP2$var$coord[rowSums(water_ACP2$var$cos2[,1:3]) > 0.05, ]

inds_selected_water2 <- as.data.frame(inds_selected_water2)
vars_selected_water2 <- as.data.frame(vars_selected_water2)

#Gráfico de los individuos con cos2 > 0.05 con 2 clusters

x <- inds_selected_water2$Dim.1
y <-inds_selected_water2$Dim.2
z <- inds_selected_water2$Dim.3

# Realizar el clustering k-means sobre los individuos seleccionados
clusters_water_3dim <- kmeans(inds_selected_water2[, 1:3], centers = 2)
inds_selected_water2$cluster <- factor(clusters_water_3dim$cluster)

ind_plot <-plot_ly(mpg, x = x, y = y, z = z, color = inds_selected_water2$cluster)%>%
  add_markers(size=1.5) %>%
  layout(scene = list(
      xaxis = list(title = "Dim1"),
      yaxis = list(title = "Dim2"),
      zaxis = list(title = "Dim3")
    )
  )
ind_plot

#Gráfico de las variables con cos2 > 0.05

# Obtener la matriz de correlación

x_var <- vars_selected_water2$Dim.1
y_var <-vars_selected_water2$Dim.2
z_var <- vars_selected_water2$Dim.3

vars <- rownames(vars_selected_water2)

var_plot <- plot_ly(x = ~c(0, x_var), y = ~c(0, y_var), z = ~c(0, z_var), 
       type = "scatter3d", mode = "lines", line = list(width = 5)) %>%
  add_trace(marker = list(size = 10)) %>%
  layout(scene = list(xaxis = list(title = "Dim.1"), yaxis = list(title = "Dim.2"), zaxis = list(title = "Dim.3")))
var_plot

# Explicación clusteres basado en la sobre posición de gráficos 

subplot(ind_plot,var_plot)



