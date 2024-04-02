#Ejercicio 1

library("FactoMineR") 
library("factoextra")
library(dplyr)

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
fviz_pca_ind(ACP, col.ind = "red", ggtheme = mi.tema, label = "none")
#Variables
fviz_pca_var(ACP, col.var = "blue", ggtheme = mi.tema)


#Elimine en R los individuos y las variables mal representadas (coseno cuadrado 
#menor al 10 %).

cos2_individuos <- (ACP$ind$cos2[,1]+ACP$ind$cos2[,2])*100
cos2_individuos

cos2_variables <- (ACP$var$cos2[,1]+ACP$var$cos2[,2])*100
cos2_variables

#Individos
fviz_pca_ind(ACP, col.ind = "red", select.ind = list(cos2 = 0.1),
             ggtheme = mi.tema, label = "none")
#Variables
fviz_pca_var(ACP, col.var = "blue", select.var = list(cos2 = 0.1),
             ggtheme = mi.tema)
