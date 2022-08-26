setwd("C:/Users/hp master/OneDrive/Escritorio/mi_data")

library(tidyverse)

tabla_clase_ancha <- read.csv("tabla_clase_ancha.csv")
tabla_clase_larga <- read.csv("tabla_clase_larga.csv")
delitos_mensuales <- read.csv("delitos_mensuales.csv")
fecha_sintomas <- read.csv("fecha_sintomas.csv")

ggplot(data = tabla_clase_ancha) +
  geom_histogram(aes(x=robos),fill = "blue",alpha=0.5,color = "black")

ggplot(data = tabla_clase_ancha) +
  geom_histogram(aes(x=robos),fill = "blue",alpha=0.5,color = "black") +
  geom_histogram(aes(x=sexuales),fill = "red",alpha=0.5,color = "black") +
  scale_x_continuous(breaks = round(seq(0, max(tabla_clase_ancha$robos), by = 125000),1))

ggplot(data = tabla_clase_larga) +
  geom_histogram(aes(x = total,fill = clase),color = "black",alpha = 0.5) +
  facet_grid(clase~.)

ggplot(data = delitos_mensuales) +
  geom_histogram(aes(x=total,y=..density..),fill = "blue",alpha=0.5,color = "black",bins = 10)+
  geom_density(aes(x=total))

ggplot(data = fecha_sintomas) +
  geom_histogram(aes(x=total,y=..density..),fill = "blue",alpha=0.5,color = "black",bins = 10)+
  geom_density(aes(x=total))


########################################################

vinos  <- read.csv("vinos.csv")
names(vinos)

mex <- vinos[vinos$country == "Mexico",]
por <- vinos[vinos$country == "Portugal",]

ggplot(data=por,aes(x = as.numeric(points))) +
  geom_histogram(aes(y=..density..),
                 fill = "yellow",
                 color = "black",
                 alpha=0.5,
                 binwidth = 1) +
  geom_density()

por1 <- data.frame(points = as.numeric(por$points),country = "Portugal")
mex1 <- data.frame(points = as.numeric(mex$points),country = "Mexico")

por_mex <- rbind(por1,mex1)

ggplot(data = por_mex) +
  geom_density(aes(points,color = country,fill=country),alpha=0.5)


medias_medianas <- data.frame(medias = tapply(as.numeric(vinos$points),
                                              vinos$country,
                                              function(x){mean(x,na.rm = TRUE)}),
                              medianas = tapply(as.numeric(vinos$points),
                                                vinos$country,
                                                function(x){median(x,na.rm=TRUE)})
)

medias_medianas$simetria <- ifelse(abs(medias_medianas$medias-medias_medianas$medianas)<0.5
                                   ,"Si","No")


esp_suiza <- vinos[vinos$country == "Spain" | vinos$country == "Switzerland",]

ggplot(data = esp_suiza) +
  geom_density(aes(points,color = country,fill=country),alpha=0.5)

# Tabla de los tres países
mis_paises <- vinos[vinos$country %in% c("Mexico","Portugal","Chile"),]

# Valor promedio de la calidad en cada país
medias = data.frame(puntaje_medio = tapply(mis_paises$points,
                                           mis_paises$country,
                                           function(x){mean(x,na.rm=TRUE)}))
medias$country <- row.names(medias)

# Graficas
ggplot() +
  geom_density(data = mis_paises,
               aes(x=points,fill=country),
               alpha = 0.5, 
               position = "identity") +
  geom_vline(data = medias,
             aes(xintercept = puntaje_medio,color=country)) +
  xlim(75,100)

tapply(mis_paises$points,
       mis_paises$country,
       function(x){sd(x,na.rm=TRUE)})



