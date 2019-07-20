# ahora haremos la simulación usando el método de montecarlo
# iniciamos dando valores a los dos candidatos
# esos valores son generados aleatoriamente.

# eliminamos el efecto de set.seed()
set.seed(NULL)

# les colocamos los valores del ejercicio anterior
a_candidato<-c(4,3,6)
b_candidato<-c(1,5,5)

# en esta simulación tomaremos en cuenta a 4 estructuras
# en realidad no sabemos cuantas estructuras existen.

# haremos 4 vectores donde almacenaremos los resultados para cada estructura
resultados_1<-c()
resultados_2<-c()
resultados_3<-c()
resultados_4<-c()

# declaramos la función de elección
escoger_candidato<-function(coeficientes, candidato_a, candidato_b){
  # la operación es para poder determinar el coeficiente
  # 0 es no conveniente, 1 es conveniencia total
  # se divide dentro de 10 porque los punteos son de 1 a 10
  a_candidato_coeficiente<-sum(candidato_a*coeficientes)/10
  b_candidato_coeficiente<-sum(candidato_b*coeficientes)/10
  
  # proceso de escoger candidato
  # devuelve 0 si el candidato seleccionado es el "a" si 1 si el candidato es el "B"
  seleccion<-ifelse(a_candidato_coeficiente>b_candidato_coeficiente, 0,
                    ifelse(a_candidato_coeficiente<b_candidato_coeficiente, 1,
                           sample(x = c(0:1),size = 1,replace = TRUE))) # si son iguales escoge uno al azar
  return(seleccion)
}

#haremos una función que genera los coeficientes de forma aleatoria
generar_coeficientes<-function(){
  coeficientes<-sample(x = c(1:3), size = 3,replace = TRUE)
  coeficientes<-coeficientes/sum(coeficientes)
  return(coeficientes)
}


# haremos una simulación de 1000 eventos
# en esta simulación vamos a variar los pesos de los coeficientes de forma aleatoria
# esto puede simular que los intereses de las estructuras cambian respecto al tiempo.
for(i in c(1:1000)){
  # coeficientes estructura "1"
  coeficientes_estructura_1<-generar_coeficientes()
  #guardamos la selección
  resultados_1<-c(resultados_1, escoger_candidato(coeficientes_estructura_1,  a_candidato, b_candidato))
  
  # coeficientes estructura "2"
  coeficientes_estructura_2<-generar_coeficientes()
  #guardamos la selección
  resultados_2<-c(resultados_2, escoger_candidato(coeficientes_estructura_2,  a_candidato, b_candidato))
  
  # coeficientes estructura "3"
  coeficientes_estructura_3<-generar_coeficientes()
  #guardamos la selección
  resultados_3<-c(resultados_3, escoger_candidato(coeficientes_estructura_3,  a_candidato, b_candidato))
  
  # coeficientes estructura "4"
  coeficientes_estructura_4<-generar_coeficientes()
  #guardamos la selección
  resultados_4<-c(resultados_4, escoger_candidato(coeficientes_estructura_4,  a_candidato, b_candidato))
}

# hacemos una función que interprete los resultados de la elección
interpretar_resultado<-function(resultado){
  valor_resultado<- sum(resultado)/length(resultado)
  respuesta<-ifelse(valor_resultado>0.61, "ganó B",
                    ifelse(valor_resultado<0.61, "ganó A","Empate entre A y B") 
                    )
  
  return(respuesta)
}

# para estructura 1
interpretar_resultado(resultados_1)
# para estructura 2
interpretar_resultado(resultados_2)
# para estructura 3
interpretar_resultado(resultados_3)
# para estrucutura 4
interpretar_resultado(resultados_4)
