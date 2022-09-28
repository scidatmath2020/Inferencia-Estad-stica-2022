##########################################
######### Regresión lineal simple

presion <- data.frame(sal=c(1.8,2.2,3.5,4.0,4.3,5.0),tension=c(100,98,110,110,112,120))
presion

resultado_regresion_lineal <- lm(tension~sal,data=presion)
resultado_regresion_lineal

summary(resultado_regresion_lineal)

#### 
#### El modelo tensión = 86.37 + 6.33*sal explica el 93% de la 
#### variabilidad del fenómeno original.

coefficients(resultado_regresion_lineal)
summary(resultado_regresion_lineal)$r.squared

confint(resultado_regresion_lineal,level=0.95)

#### El modelo tensión = 86.37 + 6.33*sal explica el 93% de la 
#### variabilidad del fenómeno original. Los coeficientes beta0 y 
#### beta1 estimados por 86.37 y 6.33, respectivamente,
#### tienen por intervalos de confianza al 95% a
#### beta0: (77.86, 94.87)
#### beta1: (04.00, 08.66)

summary(resultado_regresion_lineal)

#### El modelo tensión = 86.37 + 6.33*sal explica el 93% de la 
#### variabilidad del fenómeno original. Los coeficientes beta0 y 
#### beta1 estimados por 86.37 y 6.33, respectivamente,
#### tienen por intervalos de confianza al 95% a
#### beta0: (77.86, 94.87)
#### beta1: (04.00, 08.66)

#### Como el p-valor asociado a beta1 != 0  es pequeño, dado que
#### se obtuvo 0.0016, podemos rechazar que beta1 sea 0, y por lo 
#### tanto los efectos de la sal sobre la tensión son 
#### estadísticamente significativos.

newdata = data.frame(sal=c(4.5,5.2))

predict.lm(resultado_regresion_lineal,
           newdata,
           interval="confidence",
           level=0.95) 

predict.lm(resultado_regresion_lineal,
           newdata,
           interval="prediction",
           level=0.95)

##########################################
######### Regresión lineal múltiple

setwd("C:\\Users\\hp master\\OneDrive\\Escritorio\\mi_data")
muestra <- read.csv("muestra_calidad_vida.csv")
names(muestra)[1] <- "entidad"

####### Establecemos el modelo

names(muestra)

resultado_regresion <- lm(esp_vida ~ habitantes +
                            ingresos + analfabetismo +
                            asesinatos + universitarios +
                            heladas + area + densidad_pobl,
                            data=muestra)

summary(resultado_regresion)

summary(lm(esp_vida ~ ingresos + analfabetismo +
     asesinatos + universitarios +
     heladas + area + densidad_pobl,
   data=muestra))$adj.r.squared


#esp_vida ~ ingresos
#esp_vida ~ analfabetismo
#esp_vida ~ asesinatos

step(resultado_regresion)

modelo_simplificado <- lm(formula = esp_vida ~ ingresos + asesinatos + universitarios + 
                        area, data = muestra)

summary(modelo_simplificado)


