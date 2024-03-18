#Programación de la función recursiva 

U_n <- function(n){
  if(n == 0){
    resultado <- 5 
  } else if(n == 1){
    resultado <- -5 
  } else if(n == 2){
    resultado <- 2 
  } else{
    resultado <- 4*U_n(n-1) - 15*U_n(n-2) + U_n(n-3)
  }
  return(resultado)
}

#Prueba 

for (i in 0:10) {
  
  valor <- U_n(i)
  cat("U_", i, "=", valor, "\n")
  
}