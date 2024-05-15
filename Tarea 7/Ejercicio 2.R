#------------------------------Ejercicio 2--------------------------

# Construya un ejemplo pequeño a pie con 4 variables categóricas que permita 
# verificar el Teorema 2 visto en clase. Además, compare con el resultado de la 
# función MCA(....) del paquete FactoMineR. 

library(factoextra)
library(FactoMineR)

#Primero creamos las matrices en codigo disyuntivo de las 4 variables
sexo <- matrix(
  c(0 , 1 , 
    0 , 1 , 
    1 , 0 ,
    1 , 0 ,
    0 , 1 ,
    1 , 0 ,
    0 , 1 ,
    1 , 0 ,
    1 , 0 ,
    0 , 1 ), nrow=10,ncol = 2, byrow = T)

sexo_df <- as.data.frame(sexo)
colnames(sexo_df) <- c("Mujer","Hombre")

Ingresos <- matrix(
  c(1 , 0 , 0 , 
    0 , 0 , 1 ,
    0 , 1 , 0 ,
    1 , 0 , 0 ,
    0 , 1 , 0 ,
    0 , 0 , 1 ,
    0 , 1 , 0 ,
    0 , 0 , 1 ,
    1 , 0 , 0 ,
    1 , 0 , 0 ), nrow=10,ncol = 3, byrow = T)

Ingresos_df <- as.data.frame(Ingresos)
colnames(Ingresos_df) <- c("Bajo","Medio","Alto")

Estado_civil <- matrix(
  c(0 , 1 , 0 , 0 ,
    0 , 0 , 1 , 0 ,
    0 , 0 , 0 , 1 ,
    1 , 0 , 0 , 0 ,
    0 , 1 , 0 , 0 ,
    0 , 0 , 1 , 0 ,
    0 , 1 , 0 , 0 ,
    0 , 0 , 1 , 0 ,
    1 , 0 , 0 , 0 ,
    0 , 0 , 0 , 1 ), nrow=10,ncol = 4, byrow = T)

Estado_civil_df <- as.data.frame(Estado_civil)
colnames(Estado_civil_df) <- c("Soltero","Casado","Viudo","Divorciado")

Nivel_educativo <- matrix(
  c(0 , 0 , 1 ,
    0 , 0 , 1 ,
    0 , 1 , 0 ,
    1 , 0 , 0 ,
    0 , 1 , 0 ,
    1 , 0 , 0 ,
    0 , 1 , 0 ,
    0 , 0 , 1 ,
    1 , 0 , 0 ,
    1 , 0 , 0 ), nrow=10,ncol = 3, byrow = T)

Nivel_educativo_df <- as.data.frame(Nivel_educativo)
colnames(Nivel_educativo_df) <- c("Primaria","Secundaria","Universitaria")

# Se procede a calcular como primer metodo la matriz X la cual esta formada por la
# concatenacion de las 4 tablas de codigos disyuntivos completos.

X <- cbind(sexo_df, Ingresos_df, Estado_civil_df, Nivel_educativo_df)

AFCM_X <- CA(X, graph = FALSE)

# Coodenadas

AFCM_X$col$coord

# Valores propios

AFCM_X$eig[,1]

# Valores propios al cuadrado

AFCM_X$eig[,1]^2

# Grafico

plot(AFCM_X,invisible = "row")

# Como segundo metodo se va a realizar la matriz de Burt que es la matriz B que
# cruza las modalidades de las 4 variables. 

# Construimos la Tabla de Burt

B11 <- t(sexo) %*% sexo
B12 <- t(sexo) %*% Ingresos
B13 <- t(sexo) %*% Estado_civil
B14 <- t(sexo) %*% Nivel_educativo
FB1 <- cbind(B11,B12,B13,B14)

B21 <- t(Ingresos) %*% sexo
B22 <- t(Ingresos) %*% Ingresos
B23 <- t(Ingresos) %*% Estado_civil
B24 <- t(Ingresos) %*% Nivel_educativo
FB2 <- cbind(B21,B22,B23,B24)

B31 <- t(Estado_civil) %*% sexo
B32 <- t(Estado_civil) %*% Ingresos
B33 <- t(Estado_civil) %*% Estado_civil
B34 <- t(Estado_civil) %*% Nivel_educativo
FB3 <- cbind(B31,B32,B33,B34)

B41 <- t(Nivel_educativo) %*% sexo
B42 <- t(Nivel_educativo) %*% Ingresos
B43 <- t(Nivel_educativo) %*% Estado_civil
B44 <- t(Nivel_educativo) %*% Nivel_educativo
FB4 <- cbind(B41,B42,B43,B44)

matriz_burt <- rbind(FB1,FB2,FB3, FB4)

matriz_burt <- as.data.frame(matriz_burt)
colnames(matriz_burt) <- c("Mujer","Hombre","Bajo","Medio","Alto","Soltero",
                           "Casado","Viudo","Divorciado","Primaria",
                           "Secundaria","Universitaria")

AFCM_B <- CA(matriz_burt, graph = FALSE)

# Coodenadas

AFCM_B$col$coord

# Valores propios

AFCM_B$eig[,1]

# Grafico 

plot(AFCM_B, invisible = "row")

# Por ultimo, se van a verificar los resultados obtenidos por medio del paquete 
# FactoMiner y su funcion MCA. 

categorias_sexo <- colnames(sexo_df)
indices_sexo <- max.col(sexo_df)
sexo_original <- categorias_sexo[indices_sexo]

categorias_ingresos <- colnames(Ingresos_df)
indices_ingresos <- max.col(Ingresos_df)
ingresos_original <- categorias_ingresos[indices_ingresos]

categorias_estado_civil <- colnames(Estado_civil_df)
indices_estado_civil <- max.col(Estado_civil_df)
estado_civil_original <- categorias_estado_civil[indices_estado_civil]

categorias_nivel_educativo <- colnames(Nivel_educativo_df)
indices_nivel_educativo <- max.col(Nivel_educativo_df)
nivel_educativo_original <- categorias_nivel_educativo[indices_nivel_educativo]

datos <- data.frame(
  Sexo = sexo_original,
  Ingresos = ingresos_original,
  'Estado civil' = estado_civil_original,
  'Nivel educativo' = nivel_educativo_original
)

rownames(datos) <- c("F1","F2","F3","F4","F5","F6","F7","F8","F9","F10")
str(datos)

AFCM_MCA <- MCA(datos, ncp = 5, graph = FALSE)

#Coordenadas
AFCM_MCA$var$coord

#Valores propios 
AFCM_MCA$eig[,1]

#Grafico
fviz_mca_var(AFCM_MCA, choice = "var.cat",repel = TRUE)
