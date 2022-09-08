# Ejecutar si no lo tienes instalado
# install.packages("fitdistrplus") 

library(fitdistrplus)
library(ggplot2)

setwd("C:/Users/hp master/OneDrive/Escritorio/mi_data")

ejemplo_02 <- read.csv("ejemplo_02.csv")
ejemplo_02 <- ejemplo_02$valor


# Observamos que es una v.a. continua

descdist(ejemplo_02,discrete = FALSE, boot=500)  #bootstrap = remuestreo
 
### Notamos que los posibles candidatos son gamma, lognormal y exponencial

mis_distribuciones <- c("gamma","lnorm","exp")

ajustes <- lapply(mis_distribuciones,function(x){fitdist(ejemplo_02,x)})

ajustes[[1]]

ajustes[[2]]

ajustes[[3]]

plot(ajustes[[1]])

plot(ajustes[[2]])

plot(ajustes[[3]])

# De esto concluimos que el modelo NO es exponencial

ajustes[[1]]$aic

ajustes[[2]]$aic

### Como lo información de Akaike es menor en la lognormal que en la gamma, entonces
### la ganadora es la lognormal.

coef(ajustes[[2]])


#### Conclusión: de entre todas las distribuciones testeadas, 
#### la lognormal con media 2.68 y desviación 0.575 es la que menor 
#### información de Akaike presenta, por lo cual es la que mejor se ajusta a nuestros datos

mi_lognormal <- rlnorm(1000,meanlog=2.68,sdlog=0.57)

mi_data <- data.frame(valor_real = ejemplo_02,valor_teorico = mi_lognormal)

ggplot(data = mi_data)+
  geom_density(aes(valor_real),fill="yellow",alpha=0.5)+
  geom_density(aes(valor_teorico),fill="red",alpha=0.5)

######

plnorm(20,meanlog=2.68,sdlog=0.57) #P(una nueva observación valga a lo mas 20)






