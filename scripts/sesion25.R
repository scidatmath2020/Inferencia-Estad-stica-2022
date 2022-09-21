library(BSDA)

#########################
#########################  Ejemplo 1
#########################


muestra <- c(2150, 1950, 2170, 1860, 2050, 2120, 1920, 1850, 2230)
mu0 <- 2000
sigma <- 210
alpha=0.05


z.test(muestra,alternative="greater",mu=mu0,sigma.x=sigma,conf.level=1-alpha)

#########################
#########################  Ejemplo 3
#########################

p0 = 3/10
n = 600
exitos = 200
alpha = 0.025

###### H0: p = 3/10 (H0: p <= 3/10)
###### H1: p > 3/10 ( <, >, !=)  

binom.test(x=exitos,n=n,alternative = "greater", conf.level = 1-alpha )

#########################
#########################  Ejemplo 4
#########################

muestra <- c(2.32, 4.26, 4.02, 4.44, 3.68, 2.72, 1.90, 1.21)
alpha = 0.05
sigma0 = 0.8

#### H0: sigma <= 0.8
#### H1: sigma > 0.8

# install.packages("TeachingDemos")
library(TeachingDemos)

sigma.test(x=muestra,sigma=sigma0,alternative="greater",conf.level = 1-alpha)

###############


##### 0.8 sí está en el intervalo de confianza
##### El p-valor es pequeño

n = length(muestra)
S = sd(muestra)
sigma_0 = 0.8

x0_2 = (n-1)*S^2/sigma_0^2

x0_2

#### P(X^2>x0_2)=1-P(X^2<=x0_2)

1-pchisq(x0_2,df=n-1)
         









