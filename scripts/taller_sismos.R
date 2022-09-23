setwd("C:\\Users\\hp master\\OneDrive\\Escritorio\\mi_data")

library(fBasics)
library(agricolae)
library(ggplot2)

sismos <- read.csv("sismos_22_sep_2022.csv")

sismos <- sismos[sismos$Magnitud != "no calculable",]
sismos$Magnitud <- as.numeric(sismos$Magnitud)

sismos$mes <- substr(sismos$Fecha,4,5)

bartlett.test(Magnitud~mes,data=sismos)



#sismos <- sismos[sismos$Magnitud > 6.5,]

##### da una muestra aleatoria del mes x

muestra_mes<-function(x){
  poblacion_mes <- sismos[sismos$mes == x,]
  tamano_muestra <- as.integer(nrow(poblacion_mes)/nrow(sismos)*100)+1
  tabla_mes <- poblacion_mes[sample(1:nrow(poblacion_mes),tamano_muestra,replace=FALSE),]
  return(tabla_mes)
}

###################################################
############# GAUSSIANIDAD MENSUAL ################
###################################################


#### calcula el p-valor de la gaussianidad acerca de la magitud
#### para una muestra aleatoria del mes x

p.valor_gaussianidad_mes <- function(x){
  tabla_mes <- muestra_mes(x)
  p.valor <- fBasics::shapiroTest(tabla_mes$Magnitud)@test$p.value
  return(p.valor)
}

#### Calcula el p-valor de la gaussianidad acerca de la magitud
#### para una muestra aleatoria del mes x
#### para 50 muestras y las promedia

p.valor_gaussianidad_mes_promedio <- function(x){
  return(mean(replicate(50,p.valor_gaussianidad_mes(x))))
}

#### Devuelve el p-valor promedio de la gaussianidad acerca de la magitud
#### en cada mes

min(sapply(unique(sismos$mes),p.valor_gaussianidad_mes_promedio))

###################################################
############# HOMOCEDASTICIDAD ####################
###################################################

muestra_mes<-function(x){
  poblacion_mes <- sismos[sismos$mes == x,]
  tamano_muestra <- as.integer(nrow(poblacion_mes)/nrow(sismos)*100)+1
  tabla_mes <- poblacion_mes[sample(1:nrow(poblacion_mes),tamano_muestra,replace=TRUE),]
  return(tabla_mes)
}

#### Calculamos el p-valor de la homocedasticidad de la muestra

p.valor_homocedasticidad <- function(){
  muestras_mes <-lapply(unique(sismos$mes),muestra_mes)
  muestra <- do.call(rbind,muestras_mes)
  return(bartlett.test(Magnitud~mes,data=muestra)$p.value)
}

#### Tomamos 30 muestras y a cada una le calculamos el p-valor de 
#### la homocedasticidad. Promediamos los 30 resultados

mean(replicate(30,p.valor_homocedasticidad()))

###################################################
###################### ANOVA ######################
###################################################

resultado_anova <- aov(Magnitud~mes,data = sismos)
resultado_anova
summary(resultado_anova)


grupos_tukey <- HSD.test(resultado_anova,trt="mes",unbalanced = TRUE)$groups
grupos_tukey$mes <- row.names(grupos_tukey)
grupos_tukey$indice <- as.numeric(grupos_tukey$mes)

grupos_duncan <- duncan.test(resultado_anova,trt="mes",group = TRUE)$groups
grupos_duncan$mes <- row.names(grupos_duncan)
grupos_duncan$indice <- as.numeric(grupos_duncan$mes)

ggplot()+
  geom_point(data = sismos,aes(x=mes,y=Magnitud,color=mes),size=2,alpha=0.5) +
  geom_point(data = grupos_tukey, aes(x=mes,y=Magnitud,shape=groups,color=mes),size=5) +
  geom_segment(data = grupos_tukey,aes(x=indice-0.3,y=Magnitud,xend=indice+0.3,yend=Magnitud,color=mes)) +
  geom_line(data = grupos_tukey, aes(x=as.numeric(mes),y=Magnitud,linetype=groups),size=0.5)+
  scale_shape_manual(values=10:19)

