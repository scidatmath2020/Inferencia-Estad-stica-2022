setwd("C:/Users/hp master/OneDrive/Escritorio/mi_data")

mi_dataframe <- read.csv("online_retail2.csv")


######################################

Luis <- 32
Sara <- 25
Omar <- 21
Miguel <- 12
Raul <- 2

Raul

#####################################

Luis < 32

Luis > 32

Luis <= 32 # el símbolo <= significa menor o igual

Luis >= 32 # el símbolo >= significa mayor o igual

Luis == 32

Luis != 32

#####################################

TRUE & TRUE & FALSE

TRUE | FALSE | FALSE | FALSE

FALSE | FALSE | FALSE

# TRUE y FALSE se pueden abreviar en R con T y F

T
F

###############
### En R, el falso y el verdadero se convierten en 0 y 1
### cuando los tranformamos en número.

### Pero si convertimos de número a lógico, el 0 se vuelve
### falso y cualquier número diferente de 0 lo hace 1

as.numeric(TRUE)
as.numeric(FALSE)

as.logical(0)
as.logical(1)
as.logical(2022)
as.logical("Hola")

# Luis es menor que 40 y Omar es mayor o igual a 10
Luis < 40 & Omar >= 10

# Miguel es mayor que 40 y Luis es menor que 40
Miguel > 40 & Luis < 40

# Raul es 2 y Sara es menor que 10
Raul == 2 & Sara < 10 

# Raul es 2 y Sara es menor que 10, o Miguel es diferente de 15
(Raul == 2 & Sara <= 10) | Miguel != 15
Raul == 2 & (Sara <= 10 | Miguel != 15)

### Leyes de distributividad (A U B)^C = (A^C) U (B^C)
### (A ^ B)UC = (A U C) ^ (B U C)

####################################

names(mi_dataframe)

summary(mi_dataframe$Price)

# Añadir una columna que indique "EXTREMO" si los precios están 
# por debajo del primer cuantil y por arriba del tercer cuantil
# y que indique "USUAL" en caso contrario. ¿Cuántos hay de cada tipo?
# Sugerencia: Utiliza la función summary()

mi_dataframe$tipo_precio <- ifelse(mi_dataframe$Price < 1.25 | mi_dataframe$Price > 4.15,
                                   "EXTREMO","USUAL")

table(mi_dataframe$tipo_precio)

# Añadir una columna que indique "BAJO" si las cantidades son menores o iguales 
# que el primer cuantil; "MEDIO" si las cantidades se ubican entre el primer cuantil 
# y menores o iguales que el tercero; y "ALTO" si las cantidades sobrepasan 
# el tercer cuantil.
# Sugerencia: Utiliza la función summary()
  
summary(mi_dataframe$Quantity)

mi_dataframe$tipo_cantidad <- ifelse(mi_dataframe$Quantity <= 1,"BAJO",
                                     ifelse(mi_dataframe$Quantity > 1 & mi_dataframe$Quantity <= 10,"MEDIO","ALTO"))  

# Nos quedamos con la subtabla donde el tipo de cantidad es bajo
mi_dataframe_baja_cantidad <- mi_dataframe[mi_dataframe$tipo_cantidad == "BAJO",]


#################################################
#################################################
#################################################
# names(mi_dataframe)[3] <- "Descripcion"

