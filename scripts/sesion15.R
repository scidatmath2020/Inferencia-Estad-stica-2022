## X: número de intentos fallidos antes de armarla bien la primera vez
## X es geométrica con p=0.8
# a) k = 4; P(X=4)

dgeom(4,0.8)

## Interpretación
# 1) fallé en la primera; en la segunda ya la armé bien
# 2) la armé bien al primer intento
# 3) la armé bien hasta el sexto intento
# ...
# 1000) la armé bien hasta el octavo

# b) P(X>=3)=1-P(X<3)=1-P(X<=2)
1-pgeom(2,0.8)


###########################
###########################
########## Reescalamiento de gaussianas

mi_data <- data.frame(valores = rnorm(1000,mean=2,sd=3))

ggplot(data = mi_data) +
  geom_density(aes(valores),fill="yellow")

mi_data$valores_escalados = 4*mi_data$valores + 3

ggplot(data = mi_data) +
  geom_density(aes(valores),fill="yellow",alpha=0.5) +
  geom_density(aes(valores_escalados),fill="red",alpha=0.5)

mi_data$valores_multiplicados = -2*mi_data$valores

ggplot(data = mi_data) +
  geom_density(aes(valores),fill="yellow",alpha=0.5) +
  geom_density(aes(valores_multiplicados),fill="red",alpha=0.5)

########

## Ejemplo 1.
### X la duración en días del embarazo
### X~N(mu=266,sd=16)

### P(240<=X<=270)
### = P(X<=270)-P(X<=240)

pnorm(270,mean=266,sd=16)-pnorm(240,mean=266,sd=16)

#### Graficaremos una gaussiana con mu = 266 y desv = 16

abscisas = seq(200,350,by=0.5)
ordenadas = dnorm(abscisas,mean=266,sd=16)

mi_data = data.frame(abscisas,ordenadas)

ggplot(data = mi_data)+
  geom_line(aes(abscisas,ordenadas))

## Ejemplo 3.
### X la velocidad
### X~N(mu=90,sd=10)

### P(X>100)=1-P(X<100)
1-pnorm(100,mean=90,sd=10)

