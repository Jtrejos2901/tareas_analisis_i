# Programe en lenguaje R una clase que contenga un método que reciba como 
# entrada la Matriz de Confusión (para el caso 2×2) que calcule y retorne en un 
# diccionario: la Precisión Global, el Error Global, la Precisión Positiva (PP), 
# la Precisión Negativa (PN), la Proporción de Falsos Positivos (PFP), la 
# Proporción de Falsos Negativos (PFN), la Asertividad Positiva (AP) y la 
# Asertividad Negativa (AN)

indices_matriz <- function(matriz_confusion){
  VN <- matriz_confusion[1,1]
  FP <- matriz_confusion[1,2]
  FN <- matriz_confusion[2,1]
  VP <- matriz_confusion[2,2]
  
  P <- (VN+VP)/sum(matriz_confusion)
  E <- (FP+FN)/sum(matriz_confusion)
  PP <- VP/(FN+VP)
  PN <- VN/(VN+FP)
  PFP <- FP/(VN+FP)
  PFN <- FN/(FN+VP)
  AP <- VP/(FP+VP)
  AN <- VN/(VN+FN)
  
  indices <- list(
    'Precision Global' = P,
    'Error Global' = E,
    'Precisión Positiva' = PP,
    'Precisión Negativa' = PN,
    'Proporción de Falsos Positivos' = PFP,
    'Proporción de Falsos Negativos' = PFN,
    'Asertividad Positiva' = AP,
    'Asertividad Negativa' = AN
  )
  
  return(indices)
}

matriz_confusion <- matrix(c(782243, 238, 8553, 245), nrow = 2, ncol = 2, 
                           byrow = TRUE)
rownames(matriz_confusion) <- c("No", "Sí")
colnames(matriz_confusion) <- c("No", "Sí")

indices_matriz(matriz_confusion)