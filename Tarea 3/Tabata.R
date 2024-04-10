library(FactoMineR)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(plotly)
library("factoextra")

#Ejercicio 3
X <- matrix(c(8,1,0,4,6,5,6,8,7,10,4,7,8,2,5,0,3,6), nrow = 6, ncol = 3, byrow
            = TRUE)
X_inicial <- X 

#---------------------------Plano principal individuos--------------------------

plano_principal <- function(matriz){
  #Obtenemos la matriz con las coordenadas de los individuos
  medias_X <- medias(matriz)
  sd_X <- sd_poblacional(matriz)
  X <- centrar_y_reducir(matriz, medias_X, sd_X)
  X_R <- R(X)
  X.R_e <- eigen(X_R)
  V <- X.R_e$vectors
  C <- X%*%V
  
  #Convertimos la matriz en un dataframe
  C_data <- as.data.frame(C)
  col_names <- paste("Dim", 1:ncol(C_data))  # Genera nombres como "Dim 1", "Dim 2", etc.
  colnames(C_data) <- col_names
  #names(C_data) <- c("Dim 1", "Dim 2", "Dim 3")
  C_data$individuo <- seq_len(nrow(C_data))
  
  #Graficamos
  individuos <- ggplot(C_data, aes(x = `Dim 1`, y = `Dim 2`)) +
    geom_point(color = "lightblue") +  
    labs(title = "Plano principal (Individuos)") +
    geom_text(aes(label = individuo), vjust = 0, hjust = -0.5, 
              color = "lightblue") + 
    theme_minimal()
  
  return(individuos)
}

plano_principal(X)

#Comparamos con FactoMiner 
individuos_FM <- plot(X_ACP)

grid.arrange(plano_principal(X), individuos_FM, ncol = 2)

#----------------------------Círculo de correlaciones---------------------------
circulo_correlaciones <- function(matriz){
  #Obtenemos la matriz con las coordenadas de los individuos
  medias_X <- medias(matriz)
  sd_X <- sd_poblacional(matriz)
  X <- centrar_y_reducir(matriz, medias_X, sd_X)
  X_R <- R(X)
  X.R_e <- eigen(X_R)
  V <- X.R_e$vectors
  X.R_valores.propios <- X.R_e$values
  X_T <- T(V, X.R_valores.propios)
  
  #Convertimos la matriz en un dataframe y ajustamos para el gráfico
  X_T_data <- as.data.frame(X_T)
  col_names <- paste("Dim", 1:ncol(X_T_data))  # Genera nombres como "Dim 1", "Dim 2", etc.
  colnames(X_T_data) <- col_names
  #names(X_T_data) <- c("Dim 1", "Dim 2", "Dim 3")
  X_T_data$variable <- seq_len(nrow(X_T_data))
  X_T_data$`x origen` <- 0
  X_T_data$`y origen` <- 0
  
  #Sacamos la inercia
  X_I <- I(X.R_valores.propios)
  X_I_redondeados <- round(X_I, 2)
  inercia <- X_I_redondeados[1] + X_I_redondeados[2]
  
  #Graficamos
  variables0 <- ggplot(X_T_data, aes(x = `x origen`, y = `y origen`)) +
    geom_segment(aes(xend = `Dim 1`, yend = `Dim 2`), 
                 arrow = arrow(length = unit(0.2, "inches")), color = "orange") +
    geom_text(aes(x = `Dim 1`, y = `Dim 2`, label = variable), vjust = -0.5, 
              nudge_y = 0, nudge_x = 0, color ="orange") +
    labs(x = paste("Dim 1 (", X_I_redondeados[1], "%)"), 
         y = paste("Dim 2 (", X_I_redondeados[2], "%)"), 
         subtitle = paste("Inercia = ", inercia, "%")) +
    theme_minimal()
  
  variables <- variables0 + 
    geom_path(data = data.frame(x = cos(seq(0, 2 * pi, length.out = 100)),
                                y = sin(seq(0, 2 * pi, length.out = 100))),
              aes(x, y), color = "black", linewidth = 1, linetype = "dashed") +
    labs(title = "Círculo de correlaciones")
  
  return(list(variables0 = variables0, variables = variables))
}

circulo_correlaciones(X)


#Comparamos con FactoMiner
variables_FM <- plot(X_ACP, axes=c(1, 2), choix="var", col.var="orange",
                     new.plot=TRUE)

grid.arrange(circulo_correlaciones(X)[["variables"]], variables_FM, ncol = 2)

#---------------------------Grafico Dual----------------------------------------
grafico_dual <- function(graf_ind, graf_var) {
  # Obtener los graficos y las etiquetas 
  grafico_circulo <- graf_var$variables0
  data_graf_ind <- ggplot_build(graf_ind)$data[[1]]
  data_graf_ind$label <- seq_len(nrow(data_graf_ind))
  
  # Superponer los gráficos
  if(nrow(data_graf_ind) <= 10){
    grafico_final <- grafico_circulo +
      geom_point(data = data_graf_ind, aes(x, y), color = "lightblue") +
      geom_text(data = data_graf_ind, aes(x, y, label = label), 
                vjust = -0.5, hjust = -0.5, color = "lightblue")
  }else{
    grafico_final <- grafico_circulo +
      geom_point(data = data_graf_ind, aes(x, y), color = "lightblue", size=1)
  }
  
  return(grafico_final)
}

# Utilizar la función superponer_graficos para obtener el gráfico final
grafico_dual(plano_principal(X), circulo_correlaciones(X))

#Comparar con FactoExtra 
dual_FM <- fviz_pca_biplot(X_ACP,col.var = "orange",col.ind = "lightblue")
grid.arrange(grafico_dual(plano_principal(X), circulo_correlaciones(X)), 
             dual_FM, ncol = 2)

#------------------ Ejercicio 8-------------------------------------------------
#Verifique todo lo programado en los puntos anteriores con el ejemplo estudiantes.csv
# y con los datos del ejercicio 1 de la tarea anterior

#----- Estudiantes--------
estudiantes_datos <- read.table('Tarea 3/EjemploEstudiantes.csv', header=TRUE, sep=';',dec=',',row.names=1)
estudiantes_datos<- as.matrix(estudiantes_datos)
estudiantes_datos_original <- estudiantes_datos
plano_principal(estudiantes_datos)
circulo_correlaciones(estudiantes_datos)
grafico_dual(plano_principal(estudiantes_datos), 
             circulo_correlaciones(estudiantes_datos))

#------beans--------------
beans_datos <- read.csv("Tarea 3/beansV2.csv")
beans_datos <- as.matrix(beans_datos[,-17])
beans_datos_original <- beans_datos
plano_principal(beans_datos)
circulo_correlaciones(beans_datos)
grafico_dual(plano_principal(beans_datos), 
             circulo_correlaciones(beans_datos))
