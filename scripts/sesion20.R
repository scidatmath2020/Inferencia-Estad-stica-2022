setwd("C:\\Users\\hp master\\OneDrive\\Escritorio\\mi_data")
library(ggplot2)
library(fitdistrplus)

llegadas_clientes <- read.csv("tabla_llegadas_clientes.csv")

View(llegadas_clientes)
llegadas_clientes$hora_llegada


minuto_llegada <- (as.numeric(
  substr(llegadas_clientes$hora_llegada,1,2))-8)*60+
  as.numeric(substr(llegadas_clientes$hora_llegada,4,5))

minuto_llegada

tiempo_llegada <- c(0)
tiempo_llegada

for(i in 2:dim(llegadas_clientes)[1]){
  tiempo_llegada <- c(tiempo_llegada,minuto_llegada[i]-minuto_llegada[i-1])
}

tiempo_llegada

descdist(tiempo_llegada,discrete = FALSE,boot=500)

## Del gr?fico, proponemos gamma, beta, unif y de nuestra experiencia proponemos
## la exp

fitdist(tiempo_llegada,distr="unif")$aic
fitdist(tiempo_llegada,distr="gamma")$aic
fitdist(tiempo_llegada,distr="beta")$aic
fitdist(tiempo_llegada,distr="norm")$aic
fitdist(tiempo_llegada,distr="exp")$aic

coef(fitdist(tiempo_llegada,distr="exp"))

table(llegadas_clientes$clientes)/dim(llegadas_clientes)[1]

#### las llegadas siguen una distribuci?n EXPONENCIAL con par?metros r=0.1 
#### los n?meros de clientes siguen una distribuci?n con par?metros
 # #1: prob=0.30
   #2: prob=0.50
   #3: prob=0.10
   #4: prob=0.10


##############################################################

#################### Informaci?n
########### Real:
##### El negocio funciona por 600 minutos
##### Cada cliente tiene un consumo promedio de $35 con desviaci?n de $5.

########### Inferencia:
##### llegadas: exp con radio 0.1
##### n?mero de clientes: 
# clientes:    1   2   3   4
# proporci?n: 0.3 0.5 0.1 0.1


########### Generamos las llegadas de los clientes:
set.seed(2022)
simulacion_llegada <- cumsum(rexp(100,rate = 0.1))
simulacion_llegada <- simulacion_llegada[simulacion_llegada<600]

simulacion_llegada

# Utilizando la semilla 2022 tuvimos 59 clientes

########### Generamos los n?meros de clientes:

set.seed(2022)
simulacion_clientes <- sample(c(1,2,3,4),
       size = length(simulacion_llegada),
       replace = TRUE,
       prob = c(0.3,0.5,0.1,0.1))

simulacion_clientes

########### Creamos una tabla donde se guarde la simulaci?n

simulacion <- data.frame(simulacion_llegada,simulacion_clientes)

View(simulacion)

########### Simulamos el gasto en cada mesa

sum(rnorm(3,mean=35,sd=5))
sum(rnorm(1,mean=35,sd=5))
sum(rnorm(2,mean=35,sd=5))

# lapply(vector,funci?n)
# sapply(vector,funci?n)
set.seed(2022)
simulacion$gasto_total <- sapply(simulacion_clientes,function(x){sum(rnorm(x,mean=35,sd=5))})

########### Obtenemos la entrada total en el d?a

sum(simulacion$gasto_total)
########### Creamos una funci?n para realizar la simulaci?n tantas veces como queramos

SIMULADOR <- function(){
  simulacion_llegada <- cumsum(rexp(100,rate = 0.1))
  simulacion_llegada <- simulacion_llegada[simulacion_llegada<600]
  simulacion_clientes <- sample(c(1,2,3,4),
                                size = length(simulacion_llegada),
                                replace = TRUE,
                                prob = c(0.3,0.5,0.1,0.1))
  simulacion <- data.frame(simulacion_llegada,simulacion_clientes)
  simulacion$gasto_total <- sapply(simulacion_clientes,function(x){sum(rnorm(x,mean=35,sd=5))})
  return(sum(simulacion$gasto_total))
}

SIMULADOR()

########### Realizamos la simulaci?n 1000 veces

resultados <- replicate(100000,SIMULADOR())

hist(resultados)


########### Obtenemos el promedio. 

mean(resultados)

########### La entrada promedio al d?a es de $4,235.173