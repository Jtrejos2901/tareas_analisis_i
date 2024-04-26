# Del documento adjunto MDS no metrico.pdf leer la sección MDS no métrico y
# replicar en R los dos ejemplos que ahí se presentan.

#------------------------------Ejemplo 1-----------------------------------

nombres_fila_columna1 <- c("M", "B", "V", "S", "SS", "LC")

m <- matrix(c(
  0,   627, 351, 550, 488, 603,
  627, 0,   361, 1043,565, 1113,
  351, 361, 0,   567, 564, 954,
  550, 1043,567, 0,   971, 950,
  488, 565, 564, 971, 0,   713,
  603, 1113,954, 950, 713, 0), 
  nrow = 6, byrow = TRUE)

m <- as.data.frame(m)
rownames(m) <- nombres_fila_columna1
colnames(m) <- nombres_fila_columna1
m <- as.matrix(m)

#Calculamos Q
Q <- matrix(0, nrow = 6, ncol = 6)

sum1 <- c()
i <- 0
for (s in 1:6) {
  for (r in 1:6) {
    i <- i + (m[r,s])^2
    sum1[s] <- i
  }
  i <- 0 
}
  
sum2 <- c()
j <- 0
for (r in 1:6) {
  for (s in 1:6) {
    j <- j + (m[r,s])^2
    sum2[r] <- j
  }
  j <- 0 
}

sum3 <- 0
for (r in 1:6) {
  for (s in 1:6) {
    sum3 <- sum3 + (m[r,s])^2
  }
}

for (r in 1:6) {
  for (s in 1:6) {
    Q[r,s] <- -(1/2)*(((m[r,s])^2) - (1/6)*sum1[s] - (1/6)*sum2[r] + 
                        (1/6^2)*sum3)
  }
}

Q 

#Encontramos los valores y vectores propios de Q 
propios <- eigen(Q)
valores_propios <- propios$values
vectores_propios <- propios$vectors
valores_propios
vectores_propios

#Calculamos las coordenadas 



#------------------------------Ejemplo 2-----------------------------------

nombres_fila_columna2 <- c("Atlanta", "Chicago", "Denver", "Houston", 
                           "L. Angeles", "Miami", "N York", 
                           "S Francisco", "Seattle", "Washington")
aire.dist <- matrix(c(0, 587, 1212, 701, 1936, 604, 748, 2139, 218, 543,
                      587, 0, 920, 940, 1745, 1188, 713, 1858, 1737, 597,
                      1212, 920, 0, 879, 831, 1726, 1631, 949, 1021, 1494,
                      701, 940, 879, 0, 1374, 968, 1420, 1645, 1891, 1220,
                      1936, 1745, 831, 1374, 0, 2339, 2451, 347, 959, 2300,
                      604, 1188, 1726, 968, 2339, 0, 1092, 2594, 2734, 923,
                      748, 713, 1631, 1420, 2451, 1092, 0, 2571, 2408, 205,
                      2139, 1858, 949, 1645, 347, 2594, 2571, 0, 678, 2442,
                      218, 1737, 1021, 1891, 959, 2734, 2408, 678, 0, 2329,
                      543, 597, 1494, 1220, 2300, 923, 205, 2442, 2329, 0), 
                    nrow = 10, byrow = TRUE)
aire.dist <- as.data.frame(aire.dist)
rownames(aire.dist) <- nombres_fila_columna2
colnames(aire.dist) <- nombres_fila_columna2

# Se efectua un analisis clasico MDS metrico
aire.mds <- cmdscale(as.matrix(aire.dist),k=9,eig=T)
aire.mds

# Se Calculan los autovalores
aire.mds$eig

# Se normalizan los dos primeros autovalores
sum(abs(aire.mds$eig[1:2]))/sum(abs(aire.mds$eig))
sum(aire.mds$eig[1:2]^2)/sum(aire.mds$eig^2)

# Se muestran las coordenadas de las ciudades en las dos dimensiones
aire.mds$points[,1:2]

# Se dibujan las coordenadas de las ciudades en las dos dimensiones
par(pty="s")
plot(-aire.mds$points[,1],aire.mds$points[,2],type="n",xlab="Coordenada 1",
     ylab="Coordenada 2",xlim = c(-2000,1500),ylim=c(-2000,1500))
text(-aire.mds$points[,1],aire.mds$points[,2],labels=row.names(aire.dist))

#otra manera de dibujar las coordenadas 
plot(aire.mds$points[,1], aire.mds$points[,2], col = "orange", 
     xlab = "Coordenada X", ylab = "Coordenada Y", main = "Plot de Coordenadas")
text(aire.mds$points[,1], aire.mds$points[,2], 
     labels = rownames(aire.mds$points), pos = 3, cex = 0.3, col = "black")


library(MASS)
data(swiss)

# El fichero original de datos viene recogido ya en MASS:
swiss
swiss.x <- as.matrix(swiss[, -1])
swiss.dist <- dist(swiss.x)
swiss.mds <- isoMDS(swiss.dist)

# Las coordenadas son
swiss.mds$points

# Se dibujan los puntos
plot(swiss.mds$points, type="n", xlab="Coordenada 1", ylab="Coordenada 2")
text(swiss.mds$points, labels=as.character(1:nrow(swiss.x)))

#Otra manera de dibujar los puntos con los nombres de etiqueta 
coordenadas <- swiss.mds$points
nombres <- rownames(swiss.mds$points)

plot(coordenadas[,1], coordenadas[,2], col = "orange", xlab = "Coordenada X", 
     ylab = "Coordenada Y", main = "Plot de Coordenadas")
text(coordenadas[,1], coordenadas[,2], labels = nombres, pos = 4, cex = 0.3, 
     col = "black")

