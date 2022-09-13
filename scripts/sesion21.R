setwd("C:\\Users\\hp master\\OneDrive\\Escritorio\\mi_data")
#install.packages("BSDA")   Basics Statistics Data Analysis
library(BSDA)

tabla_poblacion_simulada <- read.csv("tabla_poblacion_simulada.csv") 

names(tabla_poblacion_simulada)


##### Problema 1.1
#set.seed(2022)
muestra <- sample(tabla_poblacion_simulada$sd_4,25,replace = TRUE)
media_muestral <- mean(muestra)
sigma = 4
n = 25
Z = 1.96

ext_izq <- media_muestral - Z*sigma/sqrt(n)
ext_der <- media_muestral + Z*sigma/sqrt(n)

#### Conclusión: En el caso de esta muestra, tenemos un intervalo de confianza
#### del 95% para la media poblacional dado por (17.7275,20.8635)

##### Problema 1.2

SIMULADOR <- function(n){
  muestra <- sample(tabla_poblacion_simulada$sd_4,n,replace = TRUE)
  media_muestral <- mean(muestra)
  sigma = 4
  Z = 2.575
  
  ext_izq <- media_muestral - Z*sigma/sqrt(n)
  ext_der <- media_muestral + Z*sigma/sqrt(n)
  
  return(data.frame(izq = ext_izq,der = ext_der))
}

resultados <- lapply(1:1000,function(x){SIMULADOR(25)})
tabla_resultados <- do.call(rbind,resultados)
nrow(tabla_resultados[tabla_resultados$izq<20 & tabla_resultados$der>20,  ])



