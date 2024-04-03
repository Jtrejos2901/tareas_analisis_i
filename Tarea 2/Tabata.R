#Ejercicio 1

library("FactoMineR") 
library("factoextra")
library(dplyr)
library(corrplot)
library("GGally")        

datos1 <- read.csv("Tarea 2/beansV2.csv")
mi.tema <- theme_grey() + theme(panel.border = element_rect(fill = NA,
                                                            color = "white"), 
                                plot.title = element_text(hjust = 0.5))

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
                ggtheme = mi.tema, label = "none")

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
                ggtheme = mi.tema, label = "none")
