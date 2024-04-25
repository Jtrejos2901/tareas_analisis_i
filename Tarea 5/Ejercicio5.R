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

#------------------------------Ejemplo 2---------------------------------

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