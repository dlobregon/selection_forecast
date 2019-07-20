# experimento acerca de  poder hacer el poder de selección
# son tres parámetros donde cada quién escoge según sus intereses.
# recordar que es un sistema complejo adaptativo.

# se generan de forma aleatoria los valores de forma aleatoria 
# estos candidatos son aleatorios

# por cuestiones de desmostriación colocaremos el "seed" para que nos devuelva los mismos valores
set.seed(100)

a_candidato<-sample(x = c(1:10), size = 3, replace = TRUE)
b_candidato<-sample(x = c(1:10), size = 3, replace = TRUE)

# la fórmula de selección es la siguiente:
# cada estructura del deep state tiene un intereses diversos intereses
# coeficiente1*punteo1 + coeficiente2*punteo3
# la generación de coeficientes es la siguiente:
coeficientes_estructura_a<-sample(x = c(1:3), size = 3,replace = TRUE)
# hacemos que la suma de los coeficientes sea igual a 1.
coeficientes_estructura_a<-coeficientes_estructura_a/sum(coeficientes_estructura_a)


# función para poder elegir un candidato entre ambos
escoger_candidato<-function(coeficientes, candidato_a, candidato_b){
  # la operación es para poder determinar el coeficiente
  # 0 es no conveniente, 1 es conveniencia total
  # se divide dentro de 10 porque los punteos son de 1 a 10
  a_candidato_coeficiente<-sum(candidato_a*coeficientes_estructura_a)/10
  b_candidato_coeficiente<-sum(candidato_b*coeficientes_estructura_a)/10
  
  # proceso de escoger candidato
  # devuelve 0 si el candidato seleccionado es el "a" si 1 si el candidato es el "B"
  seleccion<-ifelse(a_candidato_coeficiente>b_candidato_coeficiente, 0,
                    ifelse(a_candidato_coeficiente<b_candidato_coeficiente, 1,
                           sample(x = c(0:1),size = 1,replace = TRUE))) # si son iguales escoge uno al azar
  return(seleccion)
}
# mostramos los valores
# 1 - punteos del candidato a
a_candidato # los valore serían 4 3 6
# 2 - punteos del candidato a
b_candidato # los valores serían 1 5 5
# 3- coeficientes de la estructura
coeficientes_estructura_a # los valores serían 0.4285714 0.2857143 0.2857143

# procedemos a escoger el candidato
escoger_candidato(coeficientes = coeficientes_estructura_a, candidato_a = a_candidato, candidato_b = b_candidato)
# el resultado es 0, por lo que escoge al candidato "a"