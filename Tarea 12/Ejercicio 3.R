library(traineR)
library(caret)

set.seed(12)
# 1. Cargue la tabla de datos tumores.csv en R y genere en R usando la función
# createDataPartition(...) del paquete caret la tabla de testing con una 25 % de 
# los datos y con el resto de los datos genere una tabla de aprendizaje. 

datos_tumores <- read.csv("tumores.csv", header = TRUE, sep = ",", dec = ".")
datos_tumores$tipo <- as.factor(datos_tumores$tipo)

muestra <- createDataPartition(y = datos_tumores$tipo, p = 0.75, list = F)
tabla_testing <- datos_tumores[-muestra,]
tabla_aprendizaje <- datos_tumores[muestra,]
tabla_testing <- tabla_testing[,-1]
tabla_aprendizaje <- tabla_aprendizaje[,-1]

# 2. Usando Naive Bayes, LDA y QDA genere modelos predictivos para la tabla 
# de aprendizaje, puede ser que LDA y QDA generen errores.

# Naive Bayes 
modelo_bayes <- train.bayes(tipo~., data = tabla_aprendizaje)
prediccion_bayes <- predict(modelo_bayes, tabla_testing, type = "class")

# LDA
modelo_LDA <- train.lda(tipo~., data=tabla_aprendizaje)
prediccion_LDA <- predict(modelo_LDA, tabla_testing)

# QDA

# Como QDA genera errores se va a incluir un ruido en los valores de la tabla de
# aprendizaje.

valores <- matrix(runif(n = nrow(tabla_aprendizaje[,-17]) * ncol(tabla_aprendizaje[,-17]), 
                        min = 0, max = 0.01), 
                  nrow = nrow(tabla_aprendizaje[,-17]), ncol = ncol(tabla_aprendizaje[,-17]))
tabla_aprendizaje_valores <- tabla_aprendizaje[,-17] + valores
tabla_aprendizaje_valores <- cbind(tabla_aprendizaje_valores, tipo = tabla_aprendizaje[,17])

# Se genera el modelo con la nueva tabla de aprendizaje
modelo_QDA <- train.qda(tipo~., data=tabla_aprendizaje_valores)
prediccion_QDA <- predict(modelo_QDA, tabla_testing)

# 3. Para la tabla de testing calcule la matriz de confusión, la precisión 
# global, el error global y la precisión en cada de las clases. Construya una 
# tabla para los índices anteriores que permita comparar los resultados de Naive 
# Bayes, LDA y QDA con respecto a los métodos generados en las tareas anteriores 
# ¿Cuál método es mejor?

# Naive Bayes
matriz_confusion_bayes <- confusion.matrix(tabla_testing, prediccion_bayes)
general.indexes(mc = matriz_confusion_bayes)

# LDA 
matriz_confusion_LDA <- confusion.matrix(tabla_testing, prediccion_LDA)
general.indexes(mc = matriz_confusion_LDA)

# QDA 
matriz_confusion_QDA <- confusion.matrix(tabla_testing, prediccion_QDA)
general.indexes(mc = matriz_confusion_QDA)

# De los metodos de las tareas anteriores se tiene el metodo de K vecinos mas 
# cercanos 

# KNN 

modelo_KNN <- train.knn(tipo~., data = tabla_aprendizaje, 
                        kmax = floor(sqrt(nrow(tabla_aprendizaje))))
prediccion_KNN <- predict(modelo_KNN, tabla_testing, type = "class")
matriz_confusion_KNN <- confusion.matrix(tabla_testing, prediccion_KNN)
general.indexes(mc = matriz_confusion_KNN)

