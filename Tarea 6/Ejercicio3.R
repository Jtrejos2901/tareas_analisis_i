#3. El código abajo ilustra como construir una Tabla Cruzada en R a partir de 
#una tabla Individuos × Variables. Explique en detalle cada línea del siguiente 
#código y luego con la tabla resultante ejecute un AFC e interprete los 
#resultados:

# Se carga la librería MASS, la cual se utiliza para estimar modelos lineales 
# generalizados mixtos por medio de métodos de cuasi-verosimilitud penalizados 
#(PQL)
library(MASS)

#Retorna la primera parte del dataframe Cars93.
head(Cars93)  

# La función table() utiliza factores de clasificación cruzada para crear una 
# tabla de contingecia. En este caso las columnas Type y DriveTrain son las que
# deben ser utilizadas como factores para la creación de la tabla. 
# La tabla reusltante se guarda en la variable datos. 
datos <- table(Cars93$Type, Cars93$DriveTrain)

#Se imprime la variable datos, la cual muestra la frecuencia de ocurrencia de 
# cada combinación posible de valores entre las variables Type y DriveTrain.
datos

# La función class() obtiene qué clase posee el objeto datos. 
# En este caso es de tipo table. 
class(datos)

# La función unclass() se encarga de convertir el objeto datos que era de tipo 
# table en una lista sin ninguna clase específica. 
datos <- unclass(datos)

# Se obtiene la clase del objeto datos. En este caso puede ser matrix o array. 
class(datos)

# Con la función as.data.frame() se convierte el objeto datos en uno de tipo
# data.frame
datos <- as.data.frame(datos)

# Se obtiene la clase del objeto datos. En este caso data.frame.
class(datos)

# Se imprime la variable datos. 
datos

# Ahora vamos a ejecutar un AFC con la tabla cruzada resultante. 
library(FactoMineR)
library(factoextra)

AFC_datos <- CA(datos, graph = F)
AFC_datos


fviz_screeplot(AFC_datos, addlabels = TRUE)

# Variables
plot(AFC_datos, invisible = "col")

# Individuos
plot(AFC_datos, invisible = "row")

# Biplot
fviz_ca_biplot(AFC_datos, repel = TRUE)

# Cosenos cuadrados para filas 
fviz_cos2(AFC_datos, choice = "row", axes = 1:2)

# Cosenos cuadrados para columnas 
fviz_cos2(AFC_datos, choice = "col", axes = 1:2)

# Gráfico de contribuciones para las filas 
fviz_ca_row(AFC_datos, col.row = "contrib",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)

# Gráfico de contribuicones para las columnas
fviz_ca_col(AFC_datos, col.col = "contrib",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
