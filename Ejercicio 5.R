#------------------------- Ejercicio 5------------------------------------------
library(readr)
# Se carga la base de datos y se ignora la columna con la variable 'modelo'.
toyota_price <- read.csv(file = "toyota_price.csv", sep = ";", 
                         stringsAsFactors = TRUE)[,-1]

# Se muestra un resumen de la base cargada.
str(toyota_price)
head(toyota_price)

# Se genera un resumen numérico para la variable precio y se crea un boxplot con
# con el fin de analizar posible valores atípicos.
summary(toyota_price$precio)
boxplot(toyota_price$precio, xlab = "", ylab ="Precios", col = "orange", horizontal = T)

print(length(toyota_price[toyota_price$precio <= 14995,]$precio))
print(length(toyota_price$precio))

# Como se puede ver en el resumen, la media de los datos está en 12497, el 
# el mínimo en 850 y el máximo en 54991. Sin embargo, gracias al gráfico boxplot
# se puede notar que existen una gran cantidad de posibles valores atípicos. Una
# posible interpretación a esto, es que, al tratarse de precios de autos, Toyota
# como empresa cuenta con múltiples modelos con precios variados y al ser los 
# autos con precios más accesibles los de mayor demanda y aquellos con precios
# elevados los menos adquiridos por el público general, estos últimos se vuelven
# compras atípicas al compararse con todas las demás.

# Ahora, con el objetivo de realizan una matriz de correlación se omiten las  
# variables de tipo categórico.
toyota_price_numericos <- toyota_price[,-c(1,3,5)]
head(toyota_price_numericos)

# Se genera una matriz de correlación.
ggpairs(toyota_price_numericos,  upper = list(continuous = 
                                                wrap("cor", size = 2.5)))

# Como se puede observar en la matriz de correlaciones, los dos pares de 
# variables con mayores coeficientes de correlación fuerte positiva fueron el 
# tamaño del motor con el precio del vehículo y el impuesto por circulación con el 
# precio nuevamente. Una de las posibles explicaciones para la primera relación es que
# un motor más grande implica un mayor costo de producción para Toyota por lo  
# que se debe subir el precio del auto para cubrir esos costos aumentados. 
# Por otro lado, la posible explicación para la relación entre el impuesto de 
# circulación con el precio del vehículo es que los autos con precios más altos
# tienen por ley, una mayor carga impositiva, lo que implica que, a mayor precio
# mayor el monto a pagar por impuesto de circulación.