# lo que se realizó en el archivo "montecarlo_modelo_uno.R" se hizo lo siguiente:
# analizamos el proceso de selección por cada uno de las estructuras del modelo
# el problema es que no todas las estrucutras tiene la misma influencia en el modelo
# se hará una función de se hace una media ponderada en base al peso de la estructura


# se analizan tres capacidades de un político
# 1- negociación
# 2- continuidad política
# 3- Influencia en el gobierno

# definimos el número de caulidades que se van a evaluar
capacidades<-6

sandra_1<-c(6.5, 7, 6)
sandra_2<-c(8.5, 8, 8)
sandra_3<-c(8.5, 8, 8)
# agregamos las votaciones para 3 features aleatorios
sandra_4<-sample(x = c(1:10),size = 3,replace = TRUE)
sandra_5<-sample(x = c(1:10),size = 3,replace = TRUE)
sandra_6<-sample(x = c(1:10),size = 3,replace = TRUE)


alejandro_1<-c(4.5,5,5)
alejandro_2<-c(3.5,5,4.5)
alejandro_3<-c(4.5,6,5)
# agregamos las votaciones para 3 features aleatorios
alejandro_4<-sample(x = c(1:10),size = 3,replace = TRUE)
alejandro_5<-sample(x = c(1:10),size = 3,replace = TRUE)
alejandro_6<-sample(x = c(1:10),size = 3,replace = TRUE)


# les colocamos los valores del ejercicio anterior
a_candidato<-c(mean(alejandro_1), mean(alejandro_2),mean(alejandro_3), mean(alejandro_4), mean(alejandro_5), mean(alejandro_6))
b_candidato<-c(mean(sandra_1),mean(sandra_2),mean(sandra_3), mean(sandra_4), mean(sandra_5), mean(sandra_6))

# en esta simulación tomaremos en cuenta a 4 estructuras
# en realidad no sabemos cuantas estructuras existen.

# haremos 4 vectores donde almacenaremos los resultados para cada estructura
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


# haremos una simulación de 1000 eventos
# en esta simulación vamos a variar los pesos de los coeficientes de forma aleatoria
# esto puede simular que los intereses de las estructuras cambian respecto al tiempo.
for(i in c(1:50000)){
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
# procedemos a evaluar el resultado general
# el resultado general representa la porcentaje ponderado de "1's"
# en la seleccíón del sistema.

