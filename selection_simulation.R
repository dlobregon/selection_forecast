# Simulación de una selección de candidato
# esto quiere decir que no es necesariamente una elección ya que en "teoría" 
# los actores que se están simulando no afectan una elección.

# se utiliza el método de montecarlo para poder calcular la estimación de probabilidad
# de que se escoga un determinado candidato

# idea, se trata de que cada actor trate de escoger en base a quién considera mejor para 
# mantener su estado.

# factores a considerar por cada actor:
# Influencia: Capacidad de poder influenciar en las estructuras estatales.
# proyección: capacidad de continuidad como proyecto político.
# Negociación: Capacidad de lograr acuerdos con diversos sectores.


##################################
### experimento No1. 
### cada "nodo" o estrucutra se le asignará de forma aleatoria un peso para cada factor
### es decir que cada estructura tendrá una preferencia determinada para cada factor
##################################

pesos_estructura1<-sample(x = c(0:10),size = 3,replace = TRUE)
pesos_estructura1<-pesos_estructura1/10
pesos_estructura1
factores1<-sample(x = c(0:10),size = 3,replace = TRUE)
valores<-sum(factores1*pesos_estructura1)
valroes
valores
