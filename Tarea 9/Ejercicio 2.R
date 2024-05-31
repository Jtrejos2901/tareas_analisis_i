library(readr)
library(cluster)
library(ggplot2)
library(dendextend)
library(patchwork)
library(factoextra)
library(dbscan)
#----------------------------Ejercicio 2----------------------------------------
# Seembramos la semilla 
set.seed(26)

# Efectúe un análisis de k-medias realizando los siguientes pasos:
# a) Cargue la tabla de datos y ejecute un str(...), summary(...) y un dim(...), 
# verifique la correcta lectura de los datos.

datos <- read.csv("weatherAUS.csv",header=TRUE, sep=",", dec=".", row.names=1)
str(datos)
summary(datos)
dim(datos)

# b) Elimine las filas con NA usando el comando na.omit(...). 
# ¿Cuántas filas se eliminaron?

datos_limpio <- na.omit(datos)
dim(datos_limpio)
dim(datos)[1]-dim(datos_limpio)[1]
# Se eliminaron 5000 filas 

# c) Elimine de la tabla de datos la variable WindGustDir. ¿Por qué se debe 
# eliminar? ¿Qué otra alternativa se tiene en lugar de eliminarla?

datos_limpio <- datos_limpio[,-6]
# Se debe eliminar porque es una variable de tipo categórica. Otra alternativa
# en lugar de eliminarla es convertirla a código disyuntivo completo.

# d) Observe que si ejecutamos el método clustering jerárquico hclust(...) con 
# esta tabla de datos este nunca termina. ¿Por qué sucede esto?

hclust(dist(datos),method = "complete")
# Esto sucede porque estamos trabajando con una tabla de datos relativamente 
# grande puesto que, sus dimensiones son de 48934 individuos y 11 variables. 
# Entonces hay que recordar que cuando se aplica un clustering jerárquico, se 
# calculan las distancias entre cada individuo. Para este caso nunca termina 
# porque calcular las distancias entre 48934 individuos toma mucho costo 
# computacional. 

# e) Ejecute un k−medias con k = 3 con los parámetros por defecto.

k_medias <- kmeans(datos_limpio,centers=3)
head(k_medias$cluster)
k_medias$centers

# f ) Dé una interpretación de los resultados del punto anterior usando un 
# gráfico de barras.

color <- c("#CD6090","#CD6889", "#EE799F", "#FF82AB", "#DB7093",
           "pink4", "#CD919E", "#EEA9B8", "#FFB5C5", "#FFC0CB")

centros_gravedad <- k_medias$centers

centro_cluster1 <- centros_gravedad[1,]
centro_cluster1 <- data.frame("cluster" = "C1", "variable" = names(centro_cluster1),
                              "valor" = centros_gravedad[1,])
centro_cluster2 <- centros_gravedad[2,]
centro_cluster2 <- data.frame("cluster" = "C2", "variable" = names(centro_cluster2),
                              "valor" = centros_gravedad[2,])
centro_cluster3 <- centros_gravedad[3,]
centro_cluster3 <- data.frame("cluster" = "C3", "variable" = names(centro_cluster3),
                              "valor" = centros_gravedad[3,])
centros <- rbind(centro_cluster1, centro_cluster2, centro_cluster3)
for (x in unique(centros$variable)) {
  aux <- centros[centros$variable == x, "valor"]
  centros[centros$variable == x, "valor"] <- aux
}
# Cluster 1
ggplot(centros[centros$cluster == "C1", ], aes(x = variable, y = valor)) + 
  geom_bar(stat = "identity", fill = color) +
  labs(x = "Variables", y = "Cluster 1")

# Cluster 2
ggplot(centros[centros$cluster == "C2", ], aes(x = variable, y = valor)) + 
  geom_bar(stat = "identity", fill = color) +
  labs(x = "Variables", y = "Cluster 2")

# Cluster 3
ggplot(centros[centros$cluster == "C3", ], aes(x = variable, y = valor)) + 
  geom_bar(stat = "identity", fill = color) +
  labs(x = "Variables", y = "Cluster 3")

# Juntos
ggplot(centros, aes(x = variable, y = valor, fill = cluster)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("pink4", "#FFC0CB", "#FF82AB")) +
  labs(x = "Variables", y = "")

# g) Ejecute un k−medias con k = 3 con los parámetros por defecto, pero antes 
# estandarice los datos.

centrar_y_reducir <-  function(matriz){
  columnas <- ncol(matriz)
  filas <- nrow(matriz)

  for(i in 1:columnas){
    matriz[,i] <- (matriz[,i] - mean(matriz[,i])) / (sd(matriz[,i])*sqrt((filas - 1)/filas))
  }
  
  return(matriz)
}

datos_estandarizados <- centrar_y_reducir(datos_limpio)
k_medias_estandarizado <- kmeans(datos_estandarizados, centers = 3)
head(k_medias_estandarizado$cluster)
k_medias_estandarizado$centers

# Dé una interpretación de los resultados del punto anterior usando un gráfico 
# de barras. ¿Hay alguna diferencia respecto a la interpretación del punto f?

centros_gravedad_estandarizados <- k_medias_estandarizado$centers

centro_estandarizado_cluster1 <- centros_gravedad_estandarizados[1,]
centro_estandarizado_cluster1 <- data.frame("cluster" = "C1", 
                                            "variable" = names(centro_estandarizado_cluster1),
                              "valor" = centros_gravedad_estandarizados[1,])
centro_estandarizado_cluster2 <- centros_gravedad_estandarizados[2,]
centro_estandarizado_cluster2 <- data.frame("cluster" = "C2", 
                                            "variable" = names(centro_estandarizado_cluster2),
                              "valor" = centros_gravedad_estandarizados[2,])
centro_estandarizado_cluster3 <- centros_gravedad_estandarizados[3,]
centro_estandarizado_cluster3 <- data.frame("cluster" = "C3", 
                                            "variable" = names(centro_estandarizado_cluster3),
                              "valor" = centros_gravedad_estandarizados[3,])
centros_estandarizados <- rbind(centro_estandarizado_cluster1, 
                                centro_estandarizado_cluster2, 
                                centro_estandarizado_cluster3)
for (x in unique(centros_estandarizados$variable)) {
  aux_estandarizado <- centros_estandarizados[centros_estandarizados$variable == x, "valor"]
  centros_estandarizados[centros_estandarizados$variable == x, "valor"] <- aux_estandarizado
}

# Cluster 1 estandarizado
ggplot(centros_estandarizados[centros_estandarizados$cluster == "C1", ], 
       aes(x = variable, y = valor)) + 
  geom_bar(stat = "identity", fill = color) +
  labs(x = "Variables", y = "Cluster 1")

# Cluster 2 estandarizado
ggplot(centros_estandarizados[centros_estandarizados$cluster == "C2", ], 
       aes(x = variable, y = valor)) + 
  geom_bar(stat = "identity", fill = color) +
  labs(x = "Variables", y = "Cluster 2")

# Cluster 3 estandarizado
ggplot(centros_estandarizados[centros_estandarizados$cluster == "C3", ], 
       aes(x = variable, y = valor)) + 
  geom_bar(stat = "identity", fill = color) +
  labs(x = "Variables", y = "Cluster 3")

# Juntos estandarizado
ggplot(centros_estandarizados, aes(x = variable, y = valor, fill = cluster)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("pink4", "#FFC0CB", "#FF82AB")) +
  labs(x = "Variables", y = "")

# i) Ejecute un k−medias con k = 3 con iter.max=1000 y nstart=50.

k_medias_2 <- kmeans(datos_estandarizados, centers = 3, iter.max = 1000, 
                     nstart = 50)
head(k_medias_2$cluster)  
k_medias_2$centers

# j) Dé una interpretación de los resultados del punto anterior usando un 
# gráfico tipo radar.

centros_gravedad_2<- k_medias_2$centers

centro_2_cluster1 <- centros_gravedad_2[1,]
centro_2_cluster1 <- data.frame("cluster" = "C1", 
                                            "variable" = names(centro_2_cluster1),
                                            "valor" = centros_gravedad_2[1,])
centro_2_cluster2 <- centros_gravedad_2[2,]
centro_2_cluster2 <- data.frame("cluster" = "C2", 
                                            "variable" = names(centro_2_cluster2),
                                            "valor" = centros_gravedad_2[2,])
centro_2_cluster3 <- centros_gravedad_2[3,]
centro_2_cluster3 <- data.frame("cluster" = "C3", 
                                            "variable" = names(centro_2_cluster3),
                                            "valor" = centros_gravedad_2[3,])
centros_2 <- rbind(centro_2_cluster1, 
                                centro_2_cluster2, 
                                centro_2_cluster3)
for (x in unique(centros_2$variable)) {
  aux_2 <- centros_2[centros_2$variable == x, "valor"]
  aux_2 <- aux_2 - min(aux_2)
  aux_2 <- aux_2 / max(aux_2)
  centros_2[centros_2$variable == x, "valor"] <- aux_2
}

# Gráfico de radar

ggplot(data = centros_2[order(centros_2$variable), ],
       aes(x = variable, y = valor, group = cluster)) + 
  geom_point(aes(colour = cluster)) +
  geom_polygon(aes(colour = cluster, fill = cluster), alpha = 0.2) +
  ylim(-0.1, 1) + labs(x = "", y = "") + theme_minimal() +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  annotate('text', x = 0, y = c(0, 0.25, 0.5, 0.75, 1),
           label = c("0%", "25%", "50%", "75%", "100%"), color = "darkgray") +
  coord_polar()

# k) Observe que si ejecutamos el método k−medoides con k = 3 con nstart=50 con 
# esta tabla de datos este método nunca termina o tarda demasiado. ¿Por qué 
# sucede esto?

# l) Ejecute un k−medoides con k = 3 con nstart=50. Para esto tome una muestra 
# de 5 % de los datos, esto se puede lograr con el siguiente código:

numero_filas <- nrow(datos_estandarizados)
muestra <- sample(1:numero_filas,numero_filas*0.05)
datos_muestra <- datos_estandarizados[muestra,]
modelo_kmd <- pam(datos_muestra, 3, nstart = 50)

# m) Aplique el método dbscan con todos los datos ¿Termina el método? Sino 
# termina ejecútelo en la muestra del 5 %

# Con esta función podemos encontrar el valor correcto para el eps.
kNNdistplot(datos_muestra, k = 4)

# No termina de correr con todos los datos.
metodo_dbscan <- fpc::dbscan(datos_estandarizados, eps = 2, MinPts =4)

# Sí corre con la muestra del 5%.
metodo_dbscan <- fpc::dbscan(datos_muestra, eps = 2, MinPts =4)
graf <- fviz_cluster(metodo_dbscan, data = datos_muestra, stand =F,
                  ellipse = F, show.clust.cent = F,
                  geom = "point",shape = 19, palette = "Set1",
                  ggtheme = theme_minimal())
graf + labs(title = "Agrupamiento basado en densidad") +
  theme(legend.position = "bottom")

# n) Dé una interpretación de los resultados del punto anterior usando usando un 
# gráfico tipo radar. ¿Qué diferencias nota respecto a k-medias?

datos_graf_cluster <- datos_muestra
datos_graf_cluster$cluster <- metodo_dbscan$cluster

# Filtrar las observaciones que pertenecen al cluster 1
datos_graf_cluster <- datos_graf_cluster[datos_graf_cluster$cluster == 1, ]

# Calcular el centro de gravedad 
centro_cluster_dbscan <- colMeans(datos_graf_cluster[, -ncol(datos_graf_cluster)])

centro_cluster_dbscan <- data.frame("cluster" = "C1", 
                                    "variable" = names(centro_cluster_dbscan),
                              "valor" = centro_cluster_dbscan)
ggplot(data = centro_cluster_dbscan[order(centro_cluster_dbscan$variable), ],
       aes(x = variable, y = valor, group = cluster)) + 
  geom_point(aes(colour = cluster)) +
  geom_polygon(aes(colour = cluster, fill = cluster), alpha = 0.2) +
  ylim(-0.1, 1) + labs(x = "", y = "") + theme_minimal() +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  annotate('text', x = 0, y = c(0, 0.25, 0.5, 0.75, 1),
           label = c("0%", "25%", "50%", "75%", "100%"), color = "darkgray") +
  coord_polar()

# ñ) Construya el Codo de Jambu usando iter.max=100 y nstart=5, ¿cuántos 
# conglomerados (clústeres) sugiere el codo? Utilice también el método 
# silhouette de la función fviz nbclust, ¿cuántos conglomerados (clústeres) 
# sugiere este método? Para este ejercicio puede utilizar una muestra del 20 % 
# en caso de limitaciones computacionales.

muestra_20 <- sample(1:numero_filas,numero_filas*0.2)
datos_muestra_20 <- datos_estandarizados[muestra_20,]

# Codo de Jambu
InerciaIC<-rep(0,9)
for(k in 1:9) {
  grupos <- kmeans(datos_muestra_20, centers=k, nstart=5, iter.max=100)
  InerciaIC[k]<-grupos$tot.withinss
}
plot(InerciaIC,col="blue",type="b")

# Silhouette
fviz_nbclust(datos_muestra_20, kmeans, method = "silhouette",k.max = 8)