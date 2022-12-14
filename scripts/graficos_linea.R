setwd("C:/Users/hp master/OneDrive/Escritorio/mi_data")
library(ggplot2)
library(tidyverse)

fecha_sintomas <- read.csv("fecha_sintomas.csv")

ggplot(data = fecha_sintomas, aes(x = as.Date(FECHA), y = total))+
  geom_line(color = "#00AFBB", size = 0.5)+
  labs(x = "", y = "N?mero de Contagios")+
  theme_minimal()


#################################################

tabla_delitos <- read.csv("tabla_delitos.csv")

library(zoo)
tabla_delitos$Fecha <- as.yearmon(tabla_delitos$Fecha,"%b %Y")


ggplot(data = tabla_delitos) +
  geom_line(mapping = aes(Fecha,total,color=tipo))


tabla_clase_ancha <- read.csv("tabla_clase_ancha.csv")
tabla_clase_ancha$fecha <- as.yearmon(tabla_clase_ancha$fecha,"%b %Y")

ggplot(data = tabla_clase_ancha) +
  geom_line(mapping = aes(x=fecha,y=extorsion_amenazas),color = "red") +
  geom_line(mapping = aes(x=fecha,y=sexuales),color = "blue") 

tabla_clase_larga <- read.csv("tabla_clase_larga.csv")
tabla_clase_larga$fecha <- as.yearmon(tabla_clase_larga$fecha,"%b %Y")

ggplot(data = tabla_clase_larga) +
  geom_line(mapping = aes(x=fecha,y=total,color = clase)) 

#####################################################
#####################################################
#####################################################

delitos_mes <- read.csv("delitos_mensuales.csv")
delitos_mes$mes <- as.yearmon(delitos_mes$mes, "%b %Y")
delitos_mes$anio <- as.factor(delitos_mes$anio)
delitos_mes$month <- substr(delitos_mes$mes,1,4)
delitos_mes$month <- as.yearmon(delitos_mes$month, "%b")

ggplot(data = delitos_mes,mapping = aes(x = mes, y = total)) +
  geom_line(color = "blue")

ggplot(data = delitos_mes,mapping = aes(x = mes, y = total,color = anio)) +
  geom_line()

ggplot(data = delitos_mes,mapping = aes(x = month, y = total,color=anio)) +
  geom_line() 

ggplot(data = delitos_mes,mapping = aes(x = month, y = total,color=anio)) +
  geom_line() +
  facet_grid(anio~.)
  

##########################################
##########################################
##########################################

robo_calle <- data.frame(mes = delitos_mes$mes,
                            robo_en_calle = tabla_delitos[tabla_delitos$tipo == "Robo en la calle",]$total,
                            delitos_totales = delitos_mes$total)

ggplot(robo_calle) +
  geom_line(mapping = aes(x=mes,y=robo_en_calle),color = "red") +
  geom_line(mapping = aes(x=mes,y=delitos_totales),color = "blue") 
  

#####################################################



