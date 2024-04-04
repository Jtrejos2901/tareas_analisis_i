library("FactoMineR") 
library("factoextra")
library(dplyr)
library(corrplot)
library("GGally") 
library(rgl)
library(plotly)

mi.tema <- theme_grey() + theme(panel.border = element_rect(fill = NA,
                                                            color = "white"), 
                                plot.title = element_text(hjust = 0.5))

#Ejercicio 1

datos1 <- read.csv("Tarea 2/beansV2.csv")

#a) Efectúe un ACP con solo las variables numéricas.

datos1_numerico <- datos1 %>% select(-ncol(.))
ACP <- PCA(datos1_numerico, scale.unit=TRUE, ncp=4, graph = FALSE)
ACP$var
ACP$ind
#Individos
fviz_pca_ind(ACP, col.ind = "pink", ggtheme = mi.tema, label = "none")
#Variables
fviz_pca_var(ACP, col.var = "#2E9FDF", ggtheme = mi.tema)


#b) Elimine en R los individuos y las variables mal representadas (coseno cuadrado 
#menor al 10 %).

cos2_individuos <- (ACP$ind$cos2[,1]+ACP$ind$cos2[,2])*100
cos2_individuos

cos2_variables <- (ACP$var$cos2[,1]+ACP$var$cos2[,2])*100
cos2_variables

#Individos
fviz_pca_ind(ACP, col.ind = "pink", select.ind = list(cos2 = 0.1),
             ggtheme = mi.tema, label = "none")
#Variables
fviz_pca_var(ACP, col.var = "#2E9FDF", select.var = list(cos2 = 0.1),
             ggtheme = mi.tema)

#c) En el plano principal encuentre 3 clusteres.
ACP_clusters <- kmeans(ACP$ind$coord[, 1:2], centers = 3)
fviz_cluster(list(data = ACP$ind$coord[, 1:2], cluster = ACP_clusters$cluster), 
             axes = c(1, 2), geom = "point")

#d) En el circulo de correlacion determine e interprete la correlacion entre las 
#variables. Compare las correlaciones con las que se obtienen usando los graficos 
#que ofrecen paquetes de R para visualizar las correlaciones. ¿Cual calculo es 
#mas exacto? ¿Por que?

#Circulo de correlacion
fviz_pca_var(ACP,col.var="steelblue",ggtheme = mi.tema)

#Otros graficos de correlacion 
correlacion <- cor(datos1_numerico)
corrplot(correlacion)

#e) Interprete la formacion de los 3 clusteres basado en la sobre-posicion del 
#circulo y el plano.
fviz_pca_biplot(ACP, col.var = "#2E9FDF", col.ind = "pink",
                select.ind = list(cos2 = 0.1),select.var = list(cos2 = 0.1),
                ggtheme = mi.tema, geom = "point")

#f ) Convierta la variable Class a codigo disyuntivo, efectue de nuevo el ACP e 
#interprete de nuevo la formacion de los 3 clusteres basado en la sobre-posicion 
#del circulo y el plano. ¿Se gana interpretabilidad?

valores_class <- unique(datos1$Class)
valores_class
SEKER <- as.numeric(datos1$Class == "SEKER")
BARBUNYA <- as.numeric(datos1$Class == "BARBUNYA")
BOMBAY <- as.numeric(datos1$Class == "BOMBAY")
CALI <- as.numeric(datos1$Class == "CALI")
HOROZ <- as.numeric(datos1$Class == "HOROZ")
SIRA <- as.numeric(datos1$Class == "SIRA")
DERMASON <- as.numeric(datos1$Class == "DERMASON")

datos1_numerico <- cbind(datos1_numerico, SEKER, BARBUNYA, BOMBAY, CALI, HOROZ,
                         SIRA, DERMASON)
ACP_disyuntivo <- PCA(datos1_numerico, scale.unit=TRUE, ncp=4, graph = FALSE)
ACP_clusters_disyuntivo <- kmeans(ACP_disyuntivo$ind$coord[, 1:2], centers = 3)
fviz_cluster(list(data = ACP$ind$coord[, 1:2], cluster = ACP_clusters$cluster), 
             axes = c(1, 2), geom = "point")
fviz_pca_biplot(ACP_disyuntivo, col.var = "#2E9FDF", col.ind = "pink",
                select.ind = list(cos2 = 0.1),select.var = list(cos2 = 0.1),
                ggtheme = mi.tema, geom = "point")

#Ejercicio 2

#a) Cargue los datos en R, recuerde transformar la variable Potability en 
#categorica, ademas verifique bien si hay nombres de fila o no, verifique los 
#sepradores de datos y de decimales con un editor de texto.

datos2 <- read.table("Tarea 2/water_potability.csv", header=TRUE, sep=',',
                     dec='.')
datos2$Potability <- factor(datos2$Potability)
str(datos2)

#b) Con R efectue un ACP y de una interpretacion siguiendo los siguientes pasos: 

datos2_numerico <- datos2 %>% select(-ncol(.))
ACP_agua <- PCA(datos2_numerico, scale.unit=TRUE, ncp=4, graph = FALSE)

#1) elimine individuos mal representados y variables mal representadas (coseno 
#cuadrado menor al 5 %)

#Individos
fviz_pca_ind(ACP_agua, col.ind = "pink", select.ind = list(cos2 = 0.05),
             ggtheme = mi.tema, label = "none")
#Variables
fviz_pca_var(ACP_agua, col.var = "#2E9FDF", select.var = list(cos2 = 0.05),
             ggtheme = mi.tema)

#2) en el plano principal identifique un cluster en cada cuadrante
fviz_pca_ind(ACP_agua, col.ind = "pink", select.ind = list(cos2 = 0.05),
             ggtheme = mi.tema, label = "none")
#ACP_agua_clusters <- kmeans(ACP_agua$ind$coord[, 1:2], 
#                                centers = 4)
#fviz_cluster(list(data = ACP_agua$ind$coord[, 1:2], 
#                  cluster = ACP_agua_clusters$cluster), axes = c(1, 2), 
#             geom = "point")

#3) en el circulo de correlacion determine e interprete la correlacion entre las 
#variables
fviz_pca_var(ACP_agua, col.var = "#2E9FDF", select.var = list(cos2 = 0.05),
             ggtheme = mi.tema)

#4) explique la formacion de los clusteres basado en la sobre-posicion del 
#circulo y el plano.
fviz_pca_biplot(ACP_agua, col.var = "#2E9FDF", col.ind = "pink",
                select.ind = list(cos2 = 0.05),select.var = list(cos2 = 0.05),
                ggtheme = mi.tema, geom = "point")


#c) En el circulo de correlacion, usando los componentes 1 y 3, interprete la 
#correlacion entre las variables Conductivity, Trihalomethanes y Organic carbon, 
#que estan mal representadas en los componentes 1 y 2.

fviz_pca_var(ACP_agua, axes = c(1,3), col.var = "#2E9FDF", ggtheme = mi.tema)

#d) Ahora desde R convierta la variable Potability en Codigo Disyuntivo Completo 
#y repita el ACP ¿Se gana interpretabilidad al convertirla en Codigo Disyuntivo 
#Completo? En este caso use solamente 2 clusteres para la interpretacion. Use en 
#R el graficos 3D, (con algun paquete de graficacion 3D y usando 3 componentes 
#principales) para confirmar los resultados e interpretaciones. 
#¿Por que el grafico en 3D aporta resultados mas confiables?

Potability_1 <- as.numeric(datos2$Potability == "1")
Potability_0 <- as.numeric(datos2$Potability == "0")
datos2_numerico <- cbind(datos2_numerico, Potability_0, Potability_1)
ACP_agua_disyuntivo <- PCA(datos2_numerico, scale.unit=TRUE, ncp=4, 
                           graph = FALSE)

fviz_pca_biplot(ACP_agua_disyuntivo, col.var = "#2E9FDF", col.ind = "pink",
                select.ind = list(cos2 = 0.05),select.var = list(cos2 = 0.05),
                ggtheme = mi.tema, geom = "point")

ACP_agua_clusters_dis <- kmeans(ACP_agua_disyuntivo$ind$coord[, 1:2], 
                                centers = 2)
fviz_cluster(list(data = ACP_agua_disyuntivo$ind$coord[, 1:2], 
                  cluster = ACP_agua_clusters_dis$cluster), axes = c(1, 2), 
             geom = "point")

plot3d(ACP_agua_disyuntivo$ind$coord[,1], ACP_agua_disyuntivo$ind$coord[,2], 
       ACP_agua_disyuntivo$ind$coord[,3], xlab = "Dim 1", ylab = "Dim 2", 
       zlab = "Dim 3")

#grafico 3D
individuos <- ACP_agua_disyuntivo$ind$coord
x <- individuos[,1]
y <- individuos[,2]
z <- individuos[,3]

# Loadings
variables <- ACP_agua_disyuntivo$var$coord

# 3D plot
p <- plot_ly() %>%
  add_trace(x=x, y=y, z=z,
            type="scatter3d", mode="markers",
            marker = list(color=x, 
                          colorscale = c("#FFE1A1", "#683531"), 
                          opacity = 0.7, size = 2), showlegend = FALSE) 

for (k in 1:nrow(variables)) {
  x_vector <- c(0, variables[k,1])
  y_vector <- c(0, variables[k,2])
  z_vector <- c(0, variables[k,3])
  p <- p %>% add_trace(x=x_vector, y=y_vector, z=z_vector,
                       type="scatter3d", mode="lines",
                       line = list(width=2),
                       opacity = 1, name = rownames(variables)[k]) 
}
print(p)

