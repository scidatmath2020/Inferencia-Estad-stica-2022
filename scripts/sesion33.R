setwd("C:\\Users\\hp master\\OneDrive\\Escritorio\\mi_data")
library(fBasics)
library(lmtest)
library(ggplot2)
library(car)

bebes <- read.csv("bebes.csv")

bebes_modificado <- bebes[-c(9),]
  
names(bebes)

####### Paso 1: Planteamiento del modelo

resultado_regresion <- lm(Peso_actual~Edad_actual+Altura_nacer+
                            Peso_nacer+Porcentaje_cambio_peso,
                          data=bebes)	

####### Paso 2: Revisar el R^2

summary(resultado_regresion)

####### Paso 3: Simplificación del modelo

step(resultado_regresion)

modelo_simplificado <- lm(formula = Peso_actual ~ Altura_nacer + Peso_nacer, data = bebes)

#####################################
############ Diagnósticos ###########
#####################################

####### Paso 4: Gaussianidad de los residuos

modelo_simplificado$residuals

shapiro.test(modelo_simplificado$residuals)
shapiroTest(modelo_simplificado$residuals)

####### Paso 5: Homocedasticidad de los residuos
bptest(modelo_simplificado)


ggplot(data = bebes, 
       aes(modelo_simplificado$fitted.values,
           modelo_simplificado$residuals)) + 
  geom_point() + 
  geom_hline(yintercept = 0) + 
  theme_bw()

####### Paso 6: Incorrelación de los residuos

mean(replicate(100,dwt(modelo_simplificado,alternative="two.sided")$p))

####### Paso 7: Aditividad del modelo

residualPlots(modelo_simplificado,plot=TRUE)

####### Paso 8: Linealidad del modelo

crPlots(modelo_simplificado)

####### Paso 9: Observaciones anómalas

## leverage
which(hatvalues(modelo_simplificado)>2*(2+1)/9)

## outliers
outlierTest(modelo_simplificado)

## Puntos influyentes
which(cooks.distance(modelo_simplificado)>4/(9-2-2))

########## EXTRA

columnas_interes <- data.frame(altura_nacer = bebes$Altura_nacer,
                              peso_nacer = bebes$Peso_nacer,
                              num_bebe = 1:9)

ggplot()+
  geom_point(data=columnas_interes,
             aes(x=altura_nacer,y=peso_nacer,color = factor(num_bebe)))

#################################################
############ Intervalos de confianza ############
############ y significancia         ############
#################################################

####### Paso 10: intervalos de confianza
confint(modelo_simplificado)

####### Paso 11: significancia
summary(modelo_simplificado)

########### El modelo es Y=2.18 + 0.95*Altura_nacer + 3.32*Peso_nacer

nuevo_bebes <- data.frame(Altura_nacer = c(43,51),Peso_nacer =c(3.5,3.6))

predict.lm(modelo_simplificado,
           nuevo_bebes,
           interval = "prediction",
           level = 0.99)




#################################

### Si tu tabla se llama flujos, entonces

## flujos$Y <- log(flujos$Qo/flujos$Qp)
