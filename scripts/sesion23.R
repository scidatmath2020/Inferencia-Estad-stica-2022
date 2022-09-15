# install.packages("epitools")
library(epitools)

setwd("C:\\Users\\hp master\\OneDrive\\Escritorio\\mi_data")

## Ejemplo 1

binom.exact(x=592,n=985,conf.level = 0.9)

# El intervalo de confianza es (0.57,0.62). Es decir, entre el 57% y el 62% de TODOS los
# electores va a votar por dicho candidato.

binom.exact(x=592,n=985,conf.level = 0.99)

## Ejemplo 2

envipe_2022 <- read.csv("envipe_2022.csv")
envipe_2022

dim(envipe_2022)

head(envipe_2022,10)

### FAC_ELE = 1/P(ser elegido)


sum(envipe_2022$victima)
sum(envipe_2022$FAC_ELE)

victimas <- tapply(envipe_2022$FAC_ELE,envipe_2022$victima,sum)[2]
victimas

poblacion <- sum(envipe_2022$FAC_ELE)
poblacion

binom.exact(x=victimas,n=poblacion,conf.level = 0.99)

### Con un 99% de confianza, sabemos que el 26.3% de los mayores de 18 años en México 
### fueron víctimas de algún delito durante 2021

# El intervalo de confianza es (26.31,26.34). Es decir, entre el 26.31% y el 26.34% 
# de los mayores de 18 años en México 


