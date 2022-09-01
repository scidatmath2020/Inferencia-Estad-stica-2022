########## Binomial

#### Ejemplo de los zurdos 

# Sea X el número de personas zurdas que se eligieron
# X es binomial con n=5 y p=0.13
# En R, P(X=w)=dbinom(x=w,size,prob)
# En R, P(X<=w)=pbinom(w,size,prob)

# a) P(X=3)=f_X(3)

# En R:
dbinom(x=3,size=5,prob=0.13)

# b) P(X>=1) = 1-P(X<1) = 1-P(X=0)
1-dbinom(x=0,size=5,prob=0.13)

# c) P(X>=3) = 1-P(X<3) = 1-P(X<=2) = 1-F_X(2)
#    = 1-P(X=0)-P(X=1)-P(X=2)
1-pbinom(2,size=5,prob=0.13)


##################
####### Ejemplo del examen

### X=número de respuestas correctas
### p=1/5=0.2

# a) P(X<=4)

pbinom(4,size=12,prob=0.2)

# b) P(X<4)=P(X<=3)
pbinom(3,size=12,prob=0.2)

# P(X=7)
dbinom(7,size=12,prob=0.2)

###### Cada una de las 12 vale lo mismo. 
###### Apruebas con mas de 6 de calificación.

###### Cada pregunta vale 10/12*X>=6.
###### Es decir, X debe ser mayor a 7.2 para aprobar
###### Entonces X>=8 para aprobar

## P(X>=8)=1-P(X<8)=1-P(X<=7)=1-F_X(7)
1-pbinom(7,size=12,prob=0.2)

##################### Poisson

# Sea X el número de pacientes en el día.
# Sabemos que en promedio llegan 4.

# a) P(X=3)

dpois(3,lambda=4)

# b) P(X>=5)=1-P(X<5)=1-P(X<=4)
1-ppois(4,lambda=4)

# c) Probabilidad de que reciba 30 pacientes
## en una semana laboral (L-V)

#### En este caso, ahora lambda=20
### P(X=30)

dpois(30,lambda=20)

#### El número de pacientes en un día sea entre 6
#### y 10, inclusive
### P(6<=X<=10)
### = P(X=10)+P(X=9)+P(X=8)+P(X=7)+P(X=6)
### = P(X<=10)-P(X<=5)

ppois(10,lambda=4)-ppois(5,lambda=4)
#en cada 100 dias, en aproximadamente en 22
# tendria 6,7,8,9 o 10 pacientes