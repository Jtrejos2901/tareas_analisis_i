library(FactoMineR)
library(readr)

# Ejercicio 6

# Primero se genera la matriz
X <- matrix(c(8,1,0,4,6,5,6,8,7,10,4,7,8,2,5,0,3,6), nrow = 6, ncol = 3, 
            byrow = TRUE)

# Inicialmente se crea una función para centrar y reducir una matriz. Esta 
# función se genera por fuera debido a que más adelante se requerirá para la
# comparación con FactoMineR.
centrar_y_reducir <- function(matriz){
  # Se obtiene la cantidad de filas y columnas que posee la matriz.
  columnas <- dim(matriz)[[2]]
  filas <- dim(matriz)[[1]]
  
  # Se crea una nueva matriz con los datos centrados y reducidos.
  for(i in 1:columnas){
    matriz[,i] <- (matriz[,i] - mean(matriz[,i])) / (sd(matriz[,i])*sqrt((filas - 1)/filas))
  }
  
  return(matriz)
}

ACP_basdo_en_H <- function(matriz_cyr){
  # Se crea una función para generar la matriz H.
  calcular_H <- function(matriz){
    # Se retorna la matriz de Varianzas-covarianzas.
    return((1/nrow(matriz))* matriz %*% t(matriz))
  }
  
  # Se corre la función y se genera la matriz H.
  H <- calcular_H(matriz_cyr)
  
  # Se calculan los valores y vectores propios de la matriz H.
  e <- eigen(H)
  val_propios_var <- e$values
  vec_propios_var <- e$vectors

  # Dado que pude suceder que se obtengan valores propios cercanos a cero, 
  # entonces se procede a filtrar los valores y vectores propios.
  val_propios_var <- val_propios_var[val_propios_var > 1e-10]
  vec_propios_var <- vec_propios_var[,1:length(val_propios_var)]

  
  
  # Se crea una función que calcule las coordenadas de las variables.
  calcular_coordenadas <- function(matriz, vectores.propios){
    # Se obtiene la cantidad de filas y columnas que posee la matriz.
    m <- ncol(vectores.propios)
    n <- ncol(matriz)
    
    # Se crea una matriz en blanco la cual se rellenará con las coordenadas.
    coordenadas <- matrix(0,n,m)
    
    # Se rellena la matriz mediante un resultado obtenido a partir de la relación
    # de dualidad.
    for(i in 1:n){
      for(j in 1:m){
        x <- matriz_cyr[,i]
        v <- vec_propios_var[,j]
        
        coordenadas[i,j] <- t(x)%*%v/sqrt(t(x)%*%x%*%t(v)%*%v)
      }
    }
    
    return(coordenadas)
  }
  
  # Se calcula la matriz de coordenadas para las variables.
  coordenadas_var <- calcular_coordenadas(matriz_cyr, vec_propios_var)

  # Una vez que se cuenta con matriz de coordenadas, se procede a elevar al
  # cuadrado dicha matriz para obtener la matriz de cosenos cuadrados. Además se
  # compara el resultado con el obtenido con FactoMineR.
  cos2_var <- (coordenadas_var)^2
  
  # Se crea una función para calcular la contribución de las variables.
  calcular_contribuciones <- function(matriz){
    m <- ncol(matriz)
    n <- nrow(matriz)
    
    # Se genera una matriz vacía a rellenar con las contribuciones.
    resultado <- matrix(0,n,m)
    
    for(i in 1:m){
      for(j in 1:n){
        resultado[j,i] <- matriz[j,i]/sum(matriz[,i])*100
      }
    }
    
    return(resultado)
  }
  
  # Se calculan las contribuciones.
  contribuciones_var <- calcular_contribuciones(cos2_var)
  
  # Ahora que se cuenta con toda la información útil sobre las variables, se 
  # se procede a crear una función para calcular los vectores propios de los
  # individuos.
  calcular_vec_propios_ind <- function(matriz, vec_p_v, val_p_v){
    n <- ncol(matriz)
    f <- nrow(matriz)
    m <- ncol(vec_p_v)
    
    # Se genera una matriz vacía a rellenar con los vectores propios de los
    # individuos.
    resultado <- matrix(0,n,m)
    
    for(i in 1:m){
      resultado[,i] <- (t(matriz)%*%vec_p_v[,i])/sqrt(f*val_p_v[i])
    }
    
    return(resultado)
  }
  
  # Se calculan los vectores propios de los individuos.
  vec_propios_ind <- calcular_vec_propios_ind(matriz_cyr, vec_propios_var, 
                                              val_propios_var)
  
  # Se calculan las coordenadas de los individuos.
  coordenadas_ind <- matriz_cyr%*%vec_propios_ind
  
  # Se crea la funcion para el calculo de los cosenos cuadrados.
  calcular_cos2_ind <- function(C, matriz) {
    n <-nrow(matriz)
    m <-ncol(matriz)
    f <- ncol(C)
    
    
    resultado <- matrix(0, n,f)
    
    for(i in 1: n){
      suma <- 0 
      for(j in 1: m){
        suma <- suma + matriz[i,j]^2
      }
      for(r in 1: f){
        resultado[i,r] <- (C[i,r]^2)/suma
      }
    }
    return(resultado)
  }
  
  # Se calcula la matriz de cosenos cuadrados.
  cos2_ind <- calcular_cos2_ind(coordenadas_ind, matriz_cyr)
  
  # Se calcula la matriz de contribuciones.
  contribuciones_ind <- contribucion_ind(coordenadas_ind, val_propios_var)
  
  # Se calcula la inercia.
  inercia <- calcular_inercias(val_propios_var)
  
  resultado <- list(
    eigen = list(values = val_propios_var, vectors = vec_propios_ind),
    var = list(coord = coordenadas_var, cos2 = cos2_var, 
               contrib = contribuciones_var),
    ind = list(coord = coordenadas_ind, cos2 = cos2_ind, 
               contrib = contribuciones_ind)
  )
  
  return(resultado)
}

# Finalmente se corre todo el algoritmo y se compara los resultados con los
# obtenidos por FactoMineR.
X_cyr <- centrar_y_reducir(X)

ACP_a_mano <- ACP_basdo_en_H(XY_cyr)
ACP_con_FM <- PCA(XY_cyr, graph = FALSE)

# Valores propios,
ACP_a_mano$eigen$values
ACP_con_FM$eig

# Coordenadas de las variables.
ACP_a_mano$var$coord
ACP_con_FM$var$coord

# Coseno cuadrado de las variables.
ACP_a_mano$var$cos2
ACP_con_FM$var$cos2

# Contribuciones de las variables.
ACP_a_mano$var$contrib
ACP_con_FM$var$contrib

# Coordenadas de los individuos.
ACP_a_mano$ind$coord
ACP_con_FM$ind$coord

# Coseno cuadrado de los individuos.
ACP_a_mano$ind$cos2
ACP_con_FM$ind$cos2

# Contribuciones de los individuos.
ACP_a_mano$ind$contrib
ACP_con_FM$ind$contrib

# Ejercicio 7
ACP <- function(X){
  if(nrow(X) >= ncol(X)){
    return(ACP_funcion(X))
  } else{
    X_cyr <- centrar_y_reducir(X)
    return(ACP_basdo_en_H(X_cyr))
  }
}

ACP_basdo_en_H(centrar_y_reducir(estudiantes_datos))



