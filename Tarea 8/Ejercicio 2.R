library(cluster)
library(dplyr)

#-----------------------------Ejercicio 2---------------------------------------
# La tabla de datos VotosCongresoUS.csv la cual contiene 16 votos (y=Sí, n=No, 
# NS=No votó) dados por los congresistas de Estados Unidos respecto a 16 
# temáticas diferentes, además en la primera columna aparece el partido al que 
# pertenecen (Republicano o Demócrata).

# a) Ejecute una clasificación jerárquica sobre esta tabla de datos usando la 
# función daisy ya que los datos son cualitativos. Use métrica euclidean y 
#método complete (deje el resultado en la variable jer).

Datos <- read.csv("VotosCongresoUS.csv",header=TRUE, sep=",", dec=".")
Datos[] <- lapply(Datos, function(x) if(is.character(x)) as.factor(x) else x)
D <- daisy(Datos, metric = "euclidean")
jer <- hclust(D, method = "complete")

# b) Luego “corte” el ´arbol usando 3 clusteres y ejecute el siguiente codigo:

# La función cutree() corta el árbol generado por la función hclust() en varios
# grupos. Los parámetros que recibe la función son el árbol, k que es el número 
# de grupor y h que es la altura del corte. Crea un vector con el número de 
# grupo para cada individuo.
grupo <- cutree(jer, k = 3)

# Se combinan el dataframe Datos y el vector generado grupo por columnas por 
# medio de la función cbind.
NDatos <- cbind(Datos,grupo)

# Asigna a la variable cluster la columna grupo del dataframe Ndatos.
cluster <- NDatos$grupo

# La función match() devuelve un vector de las posiciones de las primeras
# coincidencias de su primer argumento en su segundo argumento. El tercer 
# argumento se refiere al valor que se devolverá en caso de que no se encuentre 
# ninguna coincidencia y debe ser de tipo int. Para este caso el prmer argumento 
# es la variable cluster y lo compara con un vector de unos y al no coincidir le 
# asigna un cero. 
sel.cluster1 <- match(cluster,c(1),0)

# Se le asigna a la variable Datos.Cluster1 el dataframe con todas las filas de 
# NDatos donde sel.cluster1 es mayor que 0. En otras palabaras, va a seleccionar
# los individuos que pertenecen al cluster 1. 
Datos.Cluster1 <- NDatos[sel.cluster1>0,]

# Retorna las dimensiones de Datos.Cluster1.
dim(Datos.Cluster1)

# Ahora se compara la variable cluster con un vector de 2. 
sel.cluster2 <- match(cluster,c(2),0)

# Se seleccionan los individuos que pertenecen al cluster 2. 
Datos.Cluster2 <- NDatos[sel.cluster2>0,]

# Se retorna las dimensiones del dataframe Datos.Cluster2. 
dim(Datos.Cluster2)

# Se compara la variable cluster con un vector de 3.
sel.cluster3 <- match(cluster,c(3),0)

# Se seleccionan los individuos que pertenecen al cluster 3.
Datos.Cluster3 <- NDatos[sel.cluster3>0,]

# Se retorna las dimensiones del dataframe Datos.Cluster3.
dim(Datos.Cluster3)

# Explique que hace el codigo anterior. Luego ejecute el siguiente codigo:

plot(Datos$Party,col=c(4,6),las=2,main="Party",xlab="Todos los Datos")
plot(Datos.Cluster1$Party,col=c(4,6),las=2,main="Party",xlab="Cluster-1")
plot(Datos.Cluster2$Party,col=c(4,6),las=2,main="Party",xlab="Cluster-2")
plot(Datos.Cluster3$Party,col=c(4,6),las=2,main="party",xlab="Cluster-3")

# Con ayuda de los gráficos anteriores y tomando en cuenta el tamaño de cada 
# cluster interprete los 3 clústeres formados.

# En total la base de datos presenta 435 individuos, estos se dividen en 
# aproximadamente 150 pertenecientes al partido republicano y el restante son 
# del partido demócrata. Al analizar por cluster se tiene que hay 232 individuos 
# que pertenecen al cluster 1 de estos, aproximadamente 70 pertenecen al partido 
# demócrata y un poco más de 150 son republicanos. En el cluster 2 hay 198 
# individuos de los cuales hay más de 150 que pertenecen al partido demócrata y 
# una cantidad muy pequeña menor a 10 son del partido republicano. Por último 
# para el cluster 3 solo hay 5 individuos, de estos 2 son del partido demócrata 
# y 3 pertenecen al partido republicano.

#-------------------------------Ejercicio 3-------------------------------------

# 3. Realice un análisis similar al del ejercicio anterior con la tabla de 
# datos CompraBicicletas.csv.

Datos <- read.csv("CompraBicicletas.csv",header=TRUE, sep=";")
Datos[] <- lapply(Datos, function(x) if(is.character(x)) as.factor(x) else x)
D <- daisy(Datos, metric = "euclidean")
jer <- hclust(D, method = "complete")
grupo <- cutree(jer, k = 3)
NDatos <- cbind(Datos,grupo)
cluster <- NDatos$grupo
sel.cluster1 <- match(cluster,c(1),0)
Datos.Cluster1 <- NDatos[sel.cluster1>0,]
sel.cluster2 <- match(cluster,c(2),0)
Datos.Cluster2 <- NDatos[sel.cluster2>0,]
sel.cluster3 <- match(cluster,c(3),0)
Datos.Cluster3 <- NDatos[sel.cluster3>0,]


plot(Datos$Gender,col=c(4,6),las=2,main="Gender",xlab="Todos los Datos")
plot(Datos.Cluster1$Gender,col=c(4,6),las=2,main="Gender",xlab="Cluster-1")
plot(Datos.Cluster2$Gender,col=c(4,6),las=2,main="Gender",xlab="Cluster-2")
plot(Datos.Cluster3$Gender,col=c(4,6),las=2,main="Gender",xlab="Cluster-3")

# Si se realiza un análisis con respecto al género de la persona, se cumple que 
# en general la base de datos cuenta con 1000 individuos divdidos con la misma 
# cantidad de hombres y mujeres. Sin embargo, al analizar por cluster se tiene 
# que el cluster 1 se encuentra formado por 518 individuos de ellos más de 300 
# son mujeres y un poco menos de 200 son hombres. El cluster 2 posee 123 
# observaciones y para este caso, contario al anterior, se tienen más hombres 
# que mujeres, aproximadamente 70 y 50 respectivamente. En e último cluster se 
# tiene el mismo comportamiento que el 2, pero con una cantidad de 100 
# individuos de género femenino y más de 300 de género masculino, para un total 
# de 359 individuos en el presente cluster. 