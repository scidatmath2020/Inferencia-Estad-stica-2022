setwd("C:\\Users\\hp master\\OneDrive\\Escritorio\\mi_data")
library(ggplot2)
library(fitdistrplus)

llegadas_clientes <- read.csv("tabla_llegadas_clientes.csv")

View(llegadas_clientes)

minuto_llegada <- (as.numeric(substr(llegadas_clientes$hora_llegada,1,2))-8)*60 +as.numeric(substr(llegadas_clientes$hora_llegada,4,5))

tiempo_llegada <- c(minuto_llegada[1])

for(i in 2:dim(llegadas_clientes)[1]){
  tiempo_llegada <- c(tiempo_llegada,minuto_llegada[i]-minuto_llegada[i-1])
}

descdist(tiempo_llegada,discrete = FALSE,boot=500)

fitdist(tiempo_llegada,distr="unif")$aic
fitdist(tiempo_llegada,distr="lnorm")$aic
fitdist(tiempo_llegada,distr="norm")$aic
fitdist(tiempo_llegada,distr="exp")$aic

table(llegadas_clientes$clientes)/dim(llegadas_clientes)[1]

#### las llegadas siguen una distribución ... con parámetros 
#### los números de clientes siguen una distribución ... con parámetros

##############################################################

#################### Información
########### Real:
##### El negocio funciona por 600 minutos
##### Cada cliente tiene un consumo promedio de $35 con desviación de $5.

########### Inferencia:
##### llegadas: 
##### número de clientes: 


########### Generamos las llegadas de los clientes:


########### Generamos los números de clientes:


########### Creamos una tabla donde se guarde la simulación


########### Simulamos el gasto en cada mesa


########### Obtenemos la entrada total en el día


########### Creamos una función para realizar la simulación tantas veces como queramos


########### Realizamos la simulación 1000 veces


########### Obtenemos el promedio. 


########### La entrada promedio al día es de ...

