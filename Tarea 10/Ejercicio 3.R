#-----------------------------------Ejercicio 3---------------------------------
library(traineR)
library(caret)

# Cargamos la base de datos 
datos_tumores <- read.csv("tumores.csv", header = TRUE, sep = ",", dec = ".")
datos_tumores$tipo <- as.factor(datos_tumores$tipo)

# 1. ¿Es un problema equilibrado? 
prediction.variable.balance(datos_tumores,"tipo")

# Es un problema desbalanceado debido a que hay muchas más observaciones que
# poseen un tumor que las que no poseen un tumor. 

# 2. Use el método de K vecinos más cercanos en el paquete traineR para generar 
# un modelo predictivo para la tabla tumores.csv usando el 75 % de los datos 
# para la tabla aprendizaje y un 25 % para la tabla testing. No olvide 
# recodificar, desde R, la variable a predecir como categórica.

# Se generar al azar una tabla de testing de con el 25% de los datos y una tabla 
# de aprendizaje del 75%. 

muestra <- createDataPartition(y = datos_tumores$tipo, p = 0.75, list = F)
tabla_testing <- datos_tumores[muestra,]
tabla_aprendizaje <- datos_tumores[-muestra,]
tabla_testing <- tabla_testing[,-1]
tabla_aprendizaje <- tabla_aprendizaje[,-1]

# Se aplica el método de K vecinos más cercanos. 
modelo <- train.knn(tipo~., data = tabla_aprendizaje, 
                    kmax = floor(sqrt(nrow(tabla_aprendizaje))))
modelo

# Se realiza la predicción.
prediccion   <- predict(modelo, tabla_testing, type = "class")
head(prediccion$prediction)

# Se realiza la matriz de confusión.
matriz_confusion <- confusion.matrix(tabla_testing, prediccion)

# Se calculan los índices de calidad de la predicción.
general.indexes(mc = matriz_confusion)

# 3. Genere un Modelo Predictivo usando K vecinos más cercanos para cada uno de 
# los siguientes núcleos: rectangular, triangular, epanechnikov, biweight, 
# triweight, cos, inv, gaussian y optimal ¿Cuál produce los mejores resultados 
# en el sentido de que predice mejor los tumores, es decir, Tumor = 1?

# Rectangular 
modelo_rectangular <- train.knn(tipo~., data = tabla_aprendizaje, 
                    kmax = floor(sqrt(nrow(tabla_aprendizaje))), 
                    kernel = "rectangular")
prediccion_rectangular   <- predict(modelo_rectangular, tabla_testing, 
                                    type = "class")
matriz_confusion_rectangular <- confusion.matrix(tabla_testing, 
                                                 prediccion_rectangular)

indices_rectangular <- general.indexes(mc = matriz_confusion_rectangular)

# Triangular
modelo_triangular <- train.knn(tipo~., data = tabla_aprendizaje, 
                                kmax = floor(sqrt(nrow(tabla_aprendizaje))), 
                                kernel = "triangular")
prediccion_triangular   <- predict(modelo_triangular, tabla_testing, 
                                    type = "class")
matriz_confusion_triangular <- confusion.matrix(tabla_testing, 
                                                 prediccion_triangular)

indices_triangular <- general.indexes(mc = matriz_confusion_triangular)

# Epanechnikov
modelo_epanechnikov <- train.knn(tipo~., data = tabla_aprendizaje, 
                               kmax = floor(sqrt(nrow(tabla_aprendizaje))), 
                               kernel = "epanechnikov")
prediccion_epanechnikov   <- predict(modelo_epanechnikov, tabla_testing, 
                                   type = "class")
matriz_confusion_epanechnikov <- confusion.matrix(tabla_testing, 
                                                prediccion_epanechnikov)

indices_epanechnikov <- general.indexes(mc = matriz_confusion_epanechnikov)

# biweight
modelo_biweight <- train.knn(tipo~., data = tabla_aprendizaje, 
                                 kmax = floor(sqrt(nrow(tabla_aprendizaje))), 
                                 kernel = "biweight")
prediccion_biweight   <- predict(modelo_biweight, tabla_testing, 
                                     type = "class")
matriz_confusion_biweight <- confusion.matrix(tabla_testing, 
                                                  prediccion_biweight)

indices_biweight <- general.indexes(mc = matriz_confusion_biweight)

# triweight
modelo_triweight <- train.knn(tipo~., data = tabla_aprendizaje, 
                             kmax = floor(sqrt(nrow(tabla_aprendizaje))), 
                             kernel = "triweight")
prediccion_triweight   <- predict(modelo_triweight, tabla_testing, 
                                 type = "class")
matriz_confusion_triweight <- confusion.matrix(tabla_testing, 
                                              prediccion_triweight)

indices_triweight <- general.indexes(mc = matriz_confusion_triweight)

# cos
modelo_cos <- train.knn(tipo~., data = tabla_aprendizaje, 
                              kmax = floor(sqrt(nrow(tabla_aprendizaje))), 
                              kernel = "cos")
prediccion_cos   <- predict(modelo_cos, tabla_testing, 
                                  type = "class")
matriz_confusion_cos <- confusion.matrix(tabla_testing, 
                                               prediccion_cos)

indices_cos <- general.indexes(mc = matriz_confusion_cos)

# inv
modelo_inv <- train.knn(tipo~., data = tabla_aprendizaje, 
                        kmax = floor(sqrt(nrow(tabla_aprendizaje))), 
                        kernel = "inv")
prediccion_inv   <- predict(modelo_inv, tabla_testing, 
                            type = "class")
matriz_confusion_inv <- confusion.matrix(tabla_testing, 
                                         prediccion_inv)

indices_inv <- general.indexes(mc = matriz_confusion_inv)

# gaussian
modelo_gaussian <- train.knn(tipo~., data = tabla_aprendizaje, 
                        kmax = floor(sqrt(nrow(tabla_aprendizaje))), 
                        kernel = "gaussian")
prediccion_gaussian   <- predict(modelo_gaussian, tabla_testing, 
                            type = "class")
matriz_confusion_gaussian <- confusion.matrix(tabla_testing, 
                                         prediccion_gaussian)

indices_gaussian <- general.indexes(mc = matriz_confusion_gaussian)

# optimal 
modelo_optimal <- train.knn(tipo~., data = tabla_aprendizaje, 
                             kmax = floor(sqrt(nrow(tabla_aprendizaje))), 
                             kernel = "optimal")
prediccion_optimal   <- predict(modelo_optimal, tabla_testing, 
                                 type = "class")
matriz_confusion_optimal <- confusion.matrix(tabla_testing, 
                                              prediccion_optimal)

indices_optimal <- general.indexes(mc = matriz_confusion_optimal)

indices <- c(rectangular = indices_rectangular$category.accuracy[2],
                triangular = indices_triangular$category.accuracy[2],
                epanechnikov = indices_epanechnikov$category.accuracy[2],
                biweight = indices_biweight$category.accuracy[2],
                triweight = indices_triweight$category.accuracy[2],
                cos = indices_cos$category.accuracy[2],
                inv = indices_inv$category.accuracy[2],
                gaussian = indices_gaussian$category.accuracy[2],
                optimal = indices_optimal$category.accuracy[2])

max(indices)

# El kernel que produce los mejores resultados en el sentido de que predice
# mejor los tumores es el biweight con un 98.86878% de casos acertados. 