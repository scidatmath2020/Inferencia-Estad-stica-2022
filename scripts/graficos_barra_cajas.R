setwd("C:/Users/hp master/OneDrive/Escritorio/mi_data")
library(ggplot2)

covid <- read.csv("covid.csv")

ggplot(data = covid) +
  geom_bar(aes(x=ENTIDAD_RES_ABR),fill = "darkgreen") +
  ggpubr::rotate_x_text()
  
tabla_clase_ancha <- read.csv("tabla_clase_ancha.csv")
tabla_clase_larga <- read.csv("tabla_clase_larga.csv")

names(tabla_clase_larga)
head(tabla_clase_ancha$fecha)
View(tabla_clase_larga)

library(zoo)

ggplot(data = tail(tabla_clase_ancha,12),aes(x=as.yearmon(fecha))) +
  geom_bar(aes(y=sexuales),stat = "identity",fill="red",alpha=0.5) + 
  geom_bar(aes(y=robos),stat = "identity",fill="blue",alpha=0.5) 

ggplot(data = tail(tabla_clase_larga,60),aes(x=as.yearmon(fecha))) +
  geom_bar(aes(y=total,fill=clase),stat="identity",alpha=0.5,position="dodge") +
  geom_line(aes(y=total,color=clase))

###########################
###########################

ggplot(data = covid) +
  geom_boxplot(aes(y=EDAD,fill=SEXO_DESCRIPCION),outlier.colour="red")

summary(covid[covid$SEXO_DESCRIPCION == "Hombres",]$EDAD)

ggplot(data = covid) +
  geom_boxplot(aes(y=EDAD,fill=SEXO_DESCRIPCION),outlier.colour="red")


ggplot(data = tabla_clase_larga,aes(y=total,fill = clase)) +
  geom_boxplot(outlier.colour="red")+
  stat_boxplot(geom ='errorbar')

tabla_robos <- tabla_clase_larga[tabla_clase_larga$clase == "robos",]

summary(tabla_robos$total)

rango = IQR(tabla_robos$total)
Q1 = quantile(tabla_robos$total)[2]
Q3 = quantile(tabla_robos$total)[4]

barrera_superior <- Q3+1.5*rango
barrera_inferior <- Q1-1.5*rango

tabla_robos[tabla_robos$total > barrera_superior,]

tabla_fraudes <- tabla_clase_larga[tabla_clase_larga$clase == "fraude",]

summary(tabla_fraudes$total)

rango = IQR(tabla_fraudes$total)
Q1 = quantile(tabla_fraudes$total)[2]
Q3 = quantile(tabla_fraudes$total)[4]

barrera_superior <- Q3+1.5*rango
barrera_inferior <- Q1-1.5*rango

tabla_fraudes[tabla_fraudes$total > barrera_superior,]

########################################

ggplot(data = tabla_robos,aes(y=total)) +
  geom_boxplot(outlier.colour="red",fill="green")+
  stat_boxplot(geom ='errorbar')+
  geom_abline(slope = 0, intercept = 398347.2, color="blue")  +
  geom_abline(slope = 0, intercept = 1693084, color = "red" ) +
  ylim(c(250000,2500000))
  


