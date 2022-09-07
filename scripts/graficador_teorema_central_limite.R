setwd("C:/Users/hp master/OneDrive/Escritorio/mi_data")
library(ggplot2)

############### Delitos mensuales
delitos_mensuales <- read.csv("delitos_mensuales.csv")
View(delitos_mensuales)

mi_data <- data.frame(valor = delitos_mensuales$total,tipo="Mensual")

set.seed(2022)
promedios <- data.frame(valor = replicate(50,mean(delitos_mensuales[sample(1:108,26,replace = FALSE),]$total)),
                        tipo = "Medias")

mi_data_aumentada <- rbind(mi_data,promedios)

ggplot(data = mi_data_aumentada) +
  geom_density(aes(valor,fill = tipo),alpha=0.5)

############## Datos de v.a. geométrica

datos <- data.frame(valor = rgeom(10000,p=0.2),tipo="real")

promedios <- data.frame(valor = replicate(5000,
                                          mean(datos[sample(1:10000,50,replace=FALSE),]$valor)),
                        tipo = "Medias")

#datos_aumentado <- rbind(datos,promedios)

ggplot() +
  geom_histogram(data = datos, aes(x=valor,y=..density..),fill="yellow",color="black",alpha=0.5) +
  geom_density(data = promedios, aes(valor),fill="red",color="black",alpha=0.5)

############## Flores iris

# iris

iris <- read.csv("iris.csv")
names(iris)

ggplot(data = iris) +
  geom_density(aes(sepal_length,fill=species),alpha=0.5)

table(iris$species)

calculador_medias_muestrales <- function(){
  setosa <- iris[iris$species == "setosa",]
  versicolor <- iris[iris$species == "versicolor",]
  virginica <- iris[iris$species == "virginica",]

  medias_muestrales_setosa <- replicate(5000,
                              mean(setosa[sample(1:50,30,replace=FALSE),]$sepal_length))
  medias_muestrales_versicolor <- replicate(5000,
                              mean(versicolor[sample(1:50,30,replace=FALSE),]$sepal_length))
  medias_muestrales_virginica <- replicate(5000,
                              mean(virginica[sample(1:50,30,replace=FALSE),]$sepal_length))
  
  medias_muestrales_setosa <- data.frame(valor = medias_muestrales_setosa,tipo = "setosa_media")
  medias_muestrales_versicolor <- data.frame(valor = medias_muestrales_versicolor,tipo = "versicolor_media")
  medias_muestrales_virginica <- data.frame(valor = medias_muestrales_virginica,tipo = "virginica_media")
  
  nva_iris <- data.frame(valor = iris$sepal_length,tipo = paste(iris$species,"real",sep="_"))
  datos = rbind(nva_iris,medias_muestrales_setosa,medias_muestrales_versicolor,medias_muestrales_virginica)
  
  return(datos)
}  

mi_data = calculador_medias_muestrales()

ggplot(data=mi_data) +
  geom_density(aes(valor,fill=tipo),alpha=0.5)


################### Setosa iris

table(mi_data$tipo)
data_setosa = mi_data[substr(mi_data$tipo,1,6)=="setosa",]

ggplot(data=data_setosa) +
  geom_density(aes(valor,fill=tipo),alpha=0.5)  


################### Iris separado por clase

mi_data$clase <- ifelse(substr(mi_data$tipo,1,6)=="setosa","setosa",
                        ifelse(substr(mi_data$tipo,1,10)=="versicolor","versicolor","virginica"))

View(mi_data)
  
ggplot(data=mi_data) +
  geom_density(aes(valor,fill=tipo),alpha=0.5) +
  facet_grid(clase~.)
View(mi_data)

################### mococha

#### Contexto: se trata de una base de datos construída a través de un muestreo que se 
#### realiza diariamente desde 2017 en la cual se hace un conteo de ciertos insectos llamados
#### áfidos (pulgones). Hay ocasiones en las que el conteo es 0 debido a que no se registraron elementos.

mococha <- read.csv("mococha.csv")

head(mococha)

ggplot()+
  geom_histogram(data = mococha,aes(x=afidos,y=..density..),fill="yellow",color="black")

promedios <- data.frame(valor = replicate(1000,mean(mococha[sample(1:dim(mococha)[1],50,replace=FALSE),]$afidos,na.rm=TRUE)),
                        tipo = "Medias")

ggplot()+
  geom_histogram(data = mococha,aes(x=afidos,y=..density..),fill="yellow",color="black",alpha=0.5)+
  geom_density(data=promedios,aes(valor),fill="red",color="black",alpha=0.5) +
  xlim(-2,10)


# dnorm(c,mean,sd) #P(X<c)