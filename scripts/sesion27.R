setwd("C:\\Users\\hp master\\OneDrive\\Escritorio\\mi_data")
library(fBasics)

presiones <- read.csv("presiones.csv")

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

bartlett.test(valor~categoria,data=presiones_larga)$p.value

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


