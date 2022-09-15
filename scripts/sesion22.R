setwd("C:\\Users\\hp master\\OneDrive\\Escritorio\\mi_data")
#install.packages("BSDA")   Basics Statistics Data Analysis
library(BSDA)

tabla_poblacion_simulada <- read.csv("tabla_poblacion_simulada.csv")

names(tabla_poblacion_simulada)


mi_muestra <- sample(tabla_poblacion_simulada$sd_desconocido,25,replace=TRUE)

t.test(mi_muestra,conf.level = 0.90)
t.test(mi_muestra,conf.level = 0.95)
t.test(mi_muestra,conf.level = 1)


tabla_poblacion_simulada$sd_desconocido

#Candidato A: tuvo a votos 
#(X,Y) al 95%

#Candidato B: tuvo b votos 
#(U,V) al 95%

#Si (X,Y) y el (U,V) tienen intersección, entonces hay empate técnico: a'<b' o b'<a'.

##########################

# Ejemplo 1.

media = 756 
z = 1.96
sigma = 35
n = 50  

iz = media - z*sigma/sqrt(n)
de = media + z*sigma/sqrt(n)

iz
de

# Ejemplo 2

data("mtcars")

mtcars$mpg  #millas por galón

t.test(mtcars$mpg,conf.level = 0.95)$conf.int


# Ejemplo 3

cobre <- c(0.1357482, 0.1548600, 0.1941724, 0.1779920, 0.1591776, 0.1399671, 0.1390927, 0.1258081, 0.1388490, 0.1137399, 0.1154759, 0.1341601, 0.1856994, 0.1603131, 0.1227789, 0.1259683, 0.1065922)
COBRE <- data.frame(valor = cobre)


library(ggplot2)

ggplot(data=COBRE) +
  geom_density(aes(valor))

t.test(cobre,conf.level = 0.90)

#### Tarea

temperatura <- read.csv("thermometry.csv")
View(temperatura)



  