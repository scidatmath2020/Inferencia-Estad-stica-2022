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

#### las llegadas siguen una distribuci?n ... con par?metros 
#### los n?meros de clientes siguen una distribuci?n ... con par?metros

##############################################################

#################### Informaci?n
########### Real:
##### El negocio funciona por 600 minutos
##### Cada cliente tiene un consumo promedio de $35 con desviaci?n de $5.

########### Inferencia:
##### llegadas: 
##### n?mero de clientes: 


########### Generamos las llegadas de los clientes:


########### Generamos los n?meros de clientes:


########### Creamos una tabla donde se guarde la simulaci?n


########### Simulamos el gasto en cada mesa


########### Obtenemos la entrada total en el d?a


########### Creamos una funci?n para realizar la simulaci?n tantas veces como queramos


########### Realizamos la simulaci?n 1000 veces


########### Obtenemos el promedio. 


########### La entrada promedio al d?a es de ...

