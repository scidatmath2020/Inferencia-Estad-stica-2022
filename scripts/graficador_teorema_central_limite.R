setwd("C:/Users/hp master/OneDrive/Escritorio/mi_data")
library(ggplot2)

############### Delitos mensuales
delitos_mensuales <- read.csv("delitos_mensuales.csv")
mi_data <- data.frame(valor = delitos_mensuales$total,tipo="Mensual")

promedios <- data.frame(valor = replicate(1000,mean(delitos_mensuales[sample(1:108,50),]$total)),
                        tipo = "Medias")

mi_data <- rbind(mi_data,promedios)

ggplot(data = mi_data) +
  geom_density(aes(valor,fill = tipo),alpha=0.5)

############## Datos de v.a. geométrica

datos <- data.frame(valor = rgeom(10000,p=0.2),tipo="real")
promedios <- data.frame(valor = replicate(5000,
                                          mean(datos[sample(1:10000,50),]$valor)),
                        tipo = "Medias")

datos <- rbind(datos,promedios)

ggplot(data = datos) +
  geom_density(aes(valor,fill = tipo),alpha=0.5)

############## Flores iris

iris <- read.csv("iris.csv")
names(iris)

ggplot(data = iris) +
  geom_density(aes(sepal_length,fill=species),alpha=0.5)

table(iris$species)

calculador_medias_muestrales <- function(){
  setosa <- iris[iris$species == "setosa",]
  versicolor <- iris[iris$species == "versicolor",]
  virginica <- iris[iris$species == "virginica",]

  medias_muestrales_setosa <- replicate(500,
                              mean(setosa[sample(1:50,30,replace=FALSE),]$sepal_length))
  medias_muestrales_versicolor <- replicate(500,
                              mean(versicolor[sample(1:50,30,replace=FALSE),]$sepal_length))
  medias_muestrales_virginica <- replicate(500,
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
  
ggplot(data=mi_data) +
  geom_density(aes(valor,fill=tipo),alpha=0.5) +
  facet_grid(clase~.)



