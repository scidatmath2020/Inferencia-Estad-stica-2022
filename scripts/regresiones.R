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

confint(modelo_simplificado)

summary(modelo_simplificado)

muestra$entidad


############

newdata = data.frame(ingresos = c(3624,4675),
                     asesinatos = c(15.1,2.3),
                     universitarios = c(41.3,57.6),
                     area =c(50708,79289))

modelo_simplificado

predict.lm(modelo_simplificado,
           newdata,
           interval = "prediction",
           level = 0.95)

newdata_extremo = data.frame(ingresos = c(0),
                     asesinatos = c(0),
                     universitarios = c(0),
                     area =c(0))

predict.lm(modelo_simplificado,
           newdata_extremo,
           interval = "prediction",
           level = 0.95)

summary(modelo_simplificado)

####################  Diagnósticos de regresión

#### Gaussianidad de los residuos

library(fBasics)
shapiroTest(modelo_simplificado$residuals)

#### Homocedasticidad
# install.packages("lmtest")
library(lmtest)

bptest(modelo_simplificado)

library(ggplot2)
ggplot(data = muestra, aes(modelo_simplificado$fitted.values, modelo_simplificado$residuals)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()

#### Independencia de los residuos
# install.packages("car")
library(car)

dwt(modelo_simplificado,alternative="two.sided")

#### Aditividad
residualPlots(modelo_simplificado,plot=TRUE)

#### Linealidad
crPlots(modelo_simplificado)

summary(modelo_simplificado)


#### Anscombe

anscombe

ans1 <- anscombe[,c(1,5)]
ans2 <- anscombe[,c(2,6)]
ans3 <- anscombe[,c(3,7)]
ans4 <- anscombe[,c(4,8)]

summary(lm(y1~x1,data=ans1))
summary(lm(y2~x2,data=ans2))
summary(lm(y3~x3,data=ans3))
summary(lm(y4~x4,data=ans4))$r.squared


modelo_ans2 <- lm(y2~x2,data=ans2)

residualPlots(modelo_ans2,plot=TRUE)
crPlots(modelo_ans2)

#### Detección de observaciones anómalas

#### Leverages

apalancamientos <- which(hatvalues(modelo_simplificado)>2*(4+1)/35)
muestra$entidad[apalancamientos]

#### Outliers de regresión

outlierTest(modelo_simplificado)

#### Observaciones influyentes

influyentes <- which(cooks.distance(modelo_simplificado)>4/(35-4-2))
influyentes
muestra$entidad[influyentes]
