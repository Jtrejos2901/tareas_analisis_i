library(readr)
library(cluster)
library(ggplot2)
#----------------------------Ejercicio 2----------------------------------------

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
  aux <- aux - min(aux)
  aux <- aux / max(aux)
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
  aux_estandarizado <- aux_estandarizado - min(aux_estandarizado)
  aux_estandarizado <- aux_estandarizado / max(aux_estandarizado)
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

# Juntos
ggplot(centros_estandarizados, aes(x = variable, y = valor, fill = cluster)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("pink4", "#FFC0CB", "#FF82AB")) +
  labs(x = "Variables", y = "")

  