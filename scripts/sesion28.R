setwd("C:\\Users\\hp master\\OneDrive\\Escritorio\\mi_data")
library(fBasics)

presiones <- read.csv("presiones.csv")

presiones

###### gaussianidad de las columnas

fBasics::shapiroTest(presiones$presion_0.0)@test$p.value
fBasics::shapiroTest(presiones$presion_0.083)@test$p.value
fBasics::shapiroTest(presiones$presion_0.29)@test$p.value
fBasics::shapiroTest(presiones$presion_0.50)@test$p.value
fBasics::shapiroTest(presiones$presion_0.86)@test$p.value

min(sapply(presiones,function(x){fBasics::shapiroTest(x)@test$p.value}))

#################################
#  Formato largo para tabla

extractor <- function(n_col){
  return(data.frame(valor = presiones[,n_col], categoria=colnames(presiones)[n_col]))
}

extracciones <- lapply(1:ncol(presiones),extractor)

presiones_larga <- do.call(rbind,extracciones)
presiones_larga

#################################
###### Homocedasticidad


bartlett.test(valor~categoria,data=presiones_larga)$p.value
?bartlett.test

#################################
#### Convertir de largo a ancho

EXTRAER <- function(nombre_col){
  presiones_larga[presiones_larga$categoria == nombre_col,]$valor
}

previo <-lapply(unique(presiones_larga$categoria),EXTRAER)

tabla_ancha <- as.data.frame(do.call(cbind,previo))

colnames(tabla_ancha) = unique(presiones_larga$categoria)

tabla_ancha

EXTRAER("presion_0.86")

###########################

###### ANOVA

names(presiones_larga)

resultado_anova <- aov(valor~categoria,data=presiones_larga)
summary(resultado_anova)

#### Concluímos que las medias NO SON TODAS IGUALES

#install.packages("agricolae")
library(agricolae)

table(presiones_larga$categoria)

duncan.test(resultado_anova,trt = "categoria",group = FALSE)$comparison
grupos_duncan <- duncan.test(resultado_anova,trt = "categoria",group = TRUE)$groups
grupos_duncan$categoria <- row.names(grupos_duncan)
grupos_duncan$indice <- 1:nrow(grupos_duncan)

#####################

library(ggplot2)

ggplot()+
  geom_point(data=presiones_larga,
             aes(x=categoria,y=valor,color=categoria),
             size=2,
             alpha=0.3) +
  geom_point(data=grupos_duncan,
             aes(x=categoria,y=valor,color=categoria),
             size=5) +
  geom_segment(data = grupos_duncan,
               aes(x=indice-0.3,y=valor,xend=indice+0.3,yend=valor,color=categoria))
