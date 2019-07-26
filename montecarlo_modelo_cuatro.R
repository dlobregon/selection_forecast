# agregamos aleatoriamente "n" features con 3 features votados por nosotros

capacidades<-12
numero_actores<-4

# los valores recogidos personalmente para sandra sobre tres features
sandra_1<-c(6.5, 7, 6)
sandra_2<-c(8.5, 8, 8)
sandra_3<-c(8.5, 8, 8)

# los valores recogidos personalmente para sandra sobre tres features
alejandro_1<-c(4.5,5,5)
alejandro_2<-c(3.5,5,4.5)
alejandro_3<-c(4.5,6,5)

generar_calificaciones_feature<-function(numero_features, numero_votos){
  valores<-c()
  for(i in c(1:numero_features)){
    generados<-sample(x = c(1:10),size = numero_votos,replace = TRUE)
    valores<-c(valores, mean(generados))
  }
  return(valores)
}


resultados_1<-c()
resultados_2<-c()
resultados_3<-c()
resultados_4<-c()
resultado_final<-c()



# declaramos la función de elección
escoger_candidato<-function(pesos, candidato_a, candidato_b){
  # la operación es para poder determinar el coeficiente
  # 0 es no conveniente, 1 es conveniencia total
  # se divide dentro de 10 porque los punteos son de 1 a 10
  a_candidato_coeficiente<-sum(candidato_a*pesos)/10
  b_candidato_coeficiente<-sum(candidato_b*pesos)/10
  
  # proceso de escoger candidato
  # devuelve 0 si el candidato seleccionado es el "a" si 1 si el candidato es el "B"
  seleccion<-ifelse(a_candidato_coeficiente>b_candidato_coeficiente, 0,
                    ifelse(a_candidato_coeficiente<b_candidato_coeficiente, 1,
                           sample(x = c(0:1),size = 1,replace = TRUE))) # si son iguales escoge uno al azar
  return(seleccion)
}

# haremos una función que genera los pesos de forma aleatoria
generar_pesos<-function(){
  pesos<-sample(x = c(1:capacidades), size = capacidades,replace = TRUE)
  pesos<-pesos/sum(pesos)
  return(pesos)
}


for(i in c(1:10000)){
  
  # creamos los pesos para cada una de las actores
  # son cuatro actores
  pesos_actores<-sample(x = c(1:numero_actores), size = numero_actores, replace = TRUE)
  pesos_actores<-pesos_actores/sum(pesos_actores)
  
  # les colocamos los valores del ejercicio anterior
  a_candidato<-c(mean(alejandro_1), mean(alejandro_2),mean(alejandro_3),generar_calificaciones_feature(capacidades-3,3))
  b_candidato<-c(mean(sandra_1),mean(sandra_2),mean(sandra_3),generar_calificaciones_feature(capacidades-3,3))
  
  # pesos actor "1"
  pesos_actor_1<-generar_pesos()
  #guardamos la selección
  resultados_1<-c(resultados_1, escoger_candidato(pesos_actor_1,  a_candidato, b_candidato))
  
  # pesos actor "2"
  pesos_actor_2<-generar_pesos()
  #guardamos la selección
  resultados_2<-c(resultados_2, escoger_candidato(pesos_actor_2,  a_candidato, b_candidato))
  
  # pesos actor "3"
  pesos_actor_3<-generar_pesos()
  #guardamos la selección
  resultados_3<-c(resultados_3, escoger_candidato(pesos_actor_3,  a_candidato, b_candidato))
  
  # pesos actor "4"
  pesos_actor_4<-generar_pesos()
  #guardamos la selección
  resultados_4<-c(resultados_4, escoger_candidato(pesos_actor_4,  a_candidato, b_candidato))
  
  # El proceso de selección general se realiza a continuación
  valores_totales<-c(
    resultados_1[i],
    resultados_2[i],
    resultados_3[i],
    resultados_4[i]
  )
  resultado_general<-sum(valores_totales*pesos_actores)
  resultado_final<-c(resultado_final, resultado_general)
}
# u es la media del resultado
u<-mean(resultado_final)

varianza<-(sum((resultado_final - u)^2)) / length(resultado_final)
std<-sqrt(varianza)
# std es nuestra estimación del error.
std

