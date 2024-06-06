---
title: Análisis de Datos I <br> Tarea 7
author:
  - "Maria Carolina Navarro Monge C05513"
  - "Tábata Picado Carmona C05961"
  - "Jose Pablo Trejos Conejo C07862"
output:
  rmdformats::robobook:
        code_folding: show
  html_document:
    toc: TRUE
    toc_depth: 2
---




# Librerías 


```r
library(cluster)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(factoextra)
```

```
## Loading required package: ggplot2
```

```
## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
```

# Ejercicio 2

**La tabla de datos VotosCongresoUS.csv la cual contiene 16 votos (y=Sí, n=No, NS=No votó) dados por los congresistas de Estados Unidos respecto a 16 temáticas diferentes, además en la primera columna aparece el partido al que pertenecen (Republicano o Demócrata).**

## a) Ejecute una clasificación jerárquica sobre esta tabla de datos usando la función daisy ya que los datos son cualitativos. Use métrica euclidean y método complete (deje el resultado en la variable jer).


```r
Datos <- read.csv("VotosCongresoUS.csv",header=TRUE, sep=",", dec=".")
Datos[] <- lapply(Datos, function(x) if(is.character(x)) as.factor(x) else x)
D <- daisy(Datos, metric = "euclidean")
```

```
## Warning in daisy(Datos, metric = "euclidean"): with mixed variables, metric
## "gower" is used automatically
```

```r
jer <- hclust(D, method = "complete")
```

## b) Luego “corte” el arbol usando 3 clusteres y ejecute el siguiente codigo y explique que hace el codigo.

La función cutree() corta el árbol generado por la función hclust() en varios grupos. Los parámetros que recibe la función son el árbol, k que es el número de grupor y h que es la altura del corte. Crea un vector con el número de grupo para cada individuo.


```r
grupo <- cutree(jer, k = 3)
```

Se combinan el dataframe Datos y el vector generado grupo por columnas por medio de la función cbind.


```r
NDatos <- cbind(Datos,grupo)
```

Se asigna a la variable cluster la columna grupo del dataframe Ndatos.


```r
cluster <- NDatos$grupo
```

La función match() devuelve un vector de las posiciones de las primeras coincidencias de su primer argumento en su segundo argumento. El tercer argumento se refiere al valor que se devolverá en caso de que no se encuentre ninguna coincidencia y debe ser de tipo int. Para este caso el prmer argumento es la variable cluster y lo compara con un vector de unos y al no coincidir le asigna un cero. 


```r
sel.cluster1 <- match(cluster,c(1),0)
```

Se le asigna a la variable Datos.Cluster1 el dataframe con todas las filas de NDatos donde sel.cluster1 es mayor que 0. En otras palabaras, va a seleccionar los individuos que pertenecen al cluster 1.


```r
Datos.Cluster1 <- NDatos[sel.cluster1>0,]
```

Retorna las dimensiones de Datos.Cluster1.


```r
dim(Datos.Cluster1)
```

```
## [1] 232  18
```

Ahora se compara la variable cluster con un vector de 2. 


```r
sel.cluster2 <- match(cluster,c(2),0)
```

Se seleccionan los individuos que pertenecen al cluster 2. 


```r
Datos.Cluster2 <- NDatos[sel.cluster2>0,]
```

Se retorna las dimensiones del dataframe Datos.Cluster2. 


```r
dim(Datos.Cluster2)
```

```
## [1] 198  18
```

Se compara la variable cluster con un vector de 3.


```r
sel.cluster3 <- match(cluster,c(3),0)
```

Se seleccionan los individuos que pertenecen al cluster 3.


```r
Datos.Cluster3 <- NDatos[sel.cluster3>0,]
```

Se retorna las dimensiones del dataframe Datos.Cluster3.


```r
dim(Datos.Cluster3)
```

```
## [1]  5 18
```

**Luego ejecute el siguiente codigo:**


```r
plot(Datos$Party,col=c(4,6),las=2,main="Party",xlab="Todos los Datos")
```

<img src="Tarea-8_files/figure-html/unnamed-chunk-15-1.png" width="768" />

```r
plot(Datos.Cluster1$Party,col=c(4,6),las=2,main="Party",xlab="Cluster-1")
```

<img src="Tarea-8_files/figure-html/unnamed-chunk-15-2.png" width="768" />

```r
plot(Datos.Cluster2$Party,col=c(4,6),las=2,main="Party",xlab="Cluster-2")
```

<img src="Tarea-8_files/figure-html/unnamed-chunk-15-3.png" width="768" />

```r
plot(Datos.Cluster3$Party,col=c(4,6),las=2,main="party",xlab="Cluster-3")
```

<img src="Tarea-8_files/figure-html/unnamed-chunk-15-4.png" width="768" />

**Con ayuda de los gráficos anteriores y tomando en cuenta el tamaño de cada cluster interprete los 3 clústeres formados.**

En total la base de datos presenta 435 individuos, estos se dividen en aproximadamente 150 pertenecientes al partido republicano y el restante son del partido demócrata. Al analizar por cluster se tiene que hay 232 individuos que pertenecen al cluster 1 de estos, aproximadamente 70 pertenecen al partido demócrata y un poco más de 150 son republicanos. En el cluster 2 hay 198 individuos de los cuales hay más de 150 que pertenecen al partido demócrata y 
una cantidad muy pequeña menor a 10 son del partido republicano. Por último para el cluster 3 solo hay 5 individuos, de estos 2 son del partido demócrata y 3 pertenecen al partido republicano.

# Ejercicio 3 

**Realice un análisis similar al del ejercicio anterior con la tabla de datos CompraBicicletas.csv.**


```r
Datos <- read.csv("CompraBicicletas.csv",header=TRUE, sep=";")
Datos[] <- lapply(Datos, function(x) if(is.character(x)) as.factor(x) else x)
D <- daisy(Datos, metric = "euclidean")
```

```
## Warning in daisy(Datos, metric = "euclidean"): with mixed variables, metric
## "gower" is used automatically
```

```r
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
```

<img src="Tarea-8_files/figure-html/unnamed-chunk-16-1.png" width="768" />

```r
plot(Datos.Cluster1$Gender,col=c(4,6),las=2,main="Gender",xlab="Cluster-1")
```

<img src="Tarea-8_files/figure-html/unnamed-chunk-16-2.png" width="768" />

```r
plot(Datos.Cluster2$Gender,col=c(4,6),las=2,main="Gender",xlab="Cluster-2")
```

<img src="Tarea-8_files/figure-html/unnamed-chunk-16-3.png" width="768" />

```r
plot(Datos.Cluster3$Gender,col=c(4,6),las=2,main="Gender",xlab="Cluster-3")
```

<img src="Tarea-8_files/figure-html/unnamed-chunk-16-4.png" width="768" />

Si se realiza un análisis con respecto al género de la persona, se cumple que en general la base de datos cuenta con 1000 individuos divdidos con la misma cantidad de hombres y mujeres. Sin embargo, al analizar por cluster se tiene que el cluster 1 se encuentra formado por 518 individuos de ellos más de 300 son mujeres y un poco menos de 200 son hombres. El cluster 2 posee 123 observaciones y para este caso, contario al anterior, se tienen más hombres que mujeres, aproximadamente 70 y 50 respectivamente. En e último cluster se tiene el mismo comportamiento que el 2, pero con una cantidad de 100 individuos de género femenino y más de 300 de género masculino, para un total de 359 individuos en el presente cluster. 

# Ejercicio 6

## a) Demuestre que la distancia de Chebychev efectivamente es una distancia.

<img src="Ejercicio 6a.png">

## b) Programe una función en R que calcula la distancia de Chebychev entre dos vectores.


```r
Chebychev <- function(vector1, vector2){
  restas <- numeric(length(vector1)-1)
  for (i in 1:length(vector1)) {
    restas[i] <- abs(vector1[i] - vector2[i]) 
  }
  restas <- unlist(restas)
  distancia <- max(restas)
  return(distancia)
}
```

## c) Programe una función en R que recibe un DataFrame calcula la matriz de distancias usando la distancia de Chebychev entre dos vectores calculada anteriormente.


```r
matriz_distancias <- function(dataframe){
  n <- nrow(dataframe)
  p <- ncol(dataframe)
  matriz <- matrix(NA, nrow = n, ncol = n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      matriz[i,j] <- Chebychev(dataframe[i,2:p], dataframe[j,2:p])
    }
  }
  
  rownames(matriz) <- dataframe[,1]
  colnames(matriz) <- dataframe[,1]
  return(matriz)
}
```

## d) Para la tabla de datos EjemploAlgoritmosRecomendacion.csv ejecute un Clustering Jerarquico de Chebychev y la agregacion Ward. Compare el resultado con el obtenido en el ejercicio 1 usando distancia euclidea.


```r
Datos6 <- read.csv("EjemploAlgoritmosRecomendacion.csv",header=TRUE, sep=";", 
                   dec=",")

distancias <- matriz_distancias(Datos6)
distancias <- as.dist(distancias)
clustering <- hclust(distancias, method = "ward.D")
fviz_dend(clustering, cex = 0.4, repel = TRUE)
```

```
## Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
## of ggplot2 3.3.4.
## ℹ The deprecated feature was likely used in the factoextra package.
##   Please report the issue at <https://github.com/kassambara/factoextra/issues>.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

<img src="Tarea-8_files/figure-html/unnamed-chunk-19-1.png" width="768" />

Se puede notar en comparación con el gráfico del ejercicio 1 hecho con distancia euclidea que al utilizar la distancia de Chebychev se forman practicamente los mismo clústeres, excepto por algunas difrencias, la altura en la que se unen los clúster sí difiere un poco entre uno y otro ya que, parece ser que las distancias son un poco menores al utilizar la distancia de Chebychev. Además, la posición de clústeres no es la misma, sin embargo, lo importante son las agrupaciones las cuales son muy similares. 