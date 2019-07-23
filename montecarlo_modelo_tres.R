# agregamos aleatoriamente "n" features con 3 features votados por nosotros

capacidades<-12
numero_estructuras<-4

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


# creamos los pesos para cada una de las estrucutras
# son cuatro estructuras
pesos_estructuras<-sample(x = c(1:4), size = 4, replace = TRUE)
pesos_estructuras<-pesos_estructuras/sum(pesos_estructuras)



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

# haremos una función que genera los coeficientes de forma aleatoria
generar_coeficientes<-function(){
  coeficientes<-sample(x = c(1:capacidades), size = capacidades,replace = TRUE)
  coeficientes<-coeficientes/sum(coeficientes)
  return(coeficientes)
}


for(i in c(1:10000)){
  # les colocamos los valores del ejercicio anterior
  a_candidato<-c(mean(alejandro_1), mean(alejandro_2),mean(alejandro_3),generar_calificaciones_feature(capacidades-3,3))
  b_candidato<-c(mean(sandra_1),mean(sandra_2),mean(sandra_3),generar_calificaciones_feature(capacidades-3,3))
  
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



# El proceso de selección general se realiza a continuación
# multiplicamos los valores por "100" para que sean representados como porcentaje
valores_totales<-c(
  (sum(resultados_1)/length(resultados_1))*100,
  (sum(resultados_2)/length(resultados_2))*100,
  (sum(resultados_3)/length(resultados_3))*100,
  (sum(resultados_4)/length(resultados_4))*100
)
resultado_general<-sum(valores_totales*pesos_estructuras)/100
resultado_general

