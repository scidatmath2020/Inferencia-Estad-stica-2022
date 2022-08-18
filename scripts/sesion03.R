mi_vector <- c("a","b","a","c","c","c")

length(unique(mi_vector))

#######################

setwd("C:/Users/hp master/OneDrive/Escritorio/mi_data")

mi_dataframe <- read.csv("online_retail2.csv")

names(mi_dataframe)

unique(mi_dataframe$Country)

table(mi_vector)

# La función sort sirve para ordenar de menor a mayor
sort(table(mi_dataframe$Country))

sort(c(4,2,5),decreasing = TRUE)

#### as.data.frame, round

dataframe_paises <- as.data.frame(table(mi_dataframe$Country))

names(dataframe_paises) <- c("Pais","Frecuencia")

dataframe_paises$fre_por <- round(dataframe_paises$Frecuencia * 100 / nrow(mi_dataframe),2)

dataframe_paises

################ la función data.frame para construir dataframes desde cero

nvo_dataframe <- data.frame(col1 = c(1,6.3,8,-2),
                            Esta_es_una_columna = c("Luis","Hugo","José","Juan"),
                            color = c("Rojo","Verde","Verde","Rojo")
                            )

nvo_dataframe

write.csv(nvo_dataframe,"C:/Users/hp master/OneDrive/Escritorio/otra/nuevo_data.csv",row.names = FALSE)

#################################

# Elegir las filas de la 100 a la 500:
mi_dataframe[100:500,]

# Elegir las filas 1,5 y 1000:
mi_dataframe[c(1,5,1000),]

# Elegir las columnas Invoice y Price
mi_dataframe[,c("Invoice","Price")]

# Elegir las columnas Price y Invoice
dos_columnas <- mi_dataframe[,c("Price","Invoice")]

# Elegir las columnas 2, 5 y 7
mi_dataframe[,c(2,5,7)]

# Elegir las columnas 2, 5, 7 y las filas 1, 5 y 1000
mi_dataframe[c(1,5,1000),c(2,5,7)]

# Elegir la subtabla donde el país es Francia
mi_dataframe[mi_dataframe$Country == "France",]

# Tabla de Francia sin columnas 2 y 4
tabla_francia_sin <- mi_dataframe[mi_dataframe$Country == "France",-c(2,4)]

####### Función summary()

summary(mi_dataframe)
summary(mi_dataframe$Quantity)

# Elegir la subtabla donde el precio es negativo
precio_neg <- mi_dataframe[mi_dataframe$Price < 0,]

precio_neg

# Cambiar el precio a 0 donde el precio es negativo

mi_dataframe[mi_dataframe$Price < 0,]$Price <- 0

summary(mi_dataframe$Price)

### Cambiando los paises donde el precio es 0 por NA (NA significa nulo)
mi_dataframe[mi_dataframe$Price == 0,]$Country <- NA

mi_dataframe[mi_dataframe$Price == 0,]


####

mi_dataframe$Price

mi_dataframe$Price[mi_dataframe$Price < 0] <- 0

# Agregar una columna al data frame de la lámina C01.2 
# de tal manera que indique TRUE si el país del cliente es Brasil

mi_dataframe$brasileiro <- ifelse(mi_dataframe$Country=="Brazil","yes","no")

table(mi_dataframe$brasileiro)

#### cuando sea de Brasil, indica "Brazil"; cuando sea de Reino Unido,
### indica "United Kingdom"; cuando no sea de ninguno de ellos, indica "world"

mi_dataframe$mundo <- ifelse(mi_dataframe$Country=="Brazil","Brazil",
                             ifelse(mi_dataframe$Country=="United Kingdom","United Kingdom","world")
                             )

table(mi_dataframe$mundo)

###################################

nvo_dataframe <- data.frame(col1 = c(1,6.3,8,-2),
                            Esta_es_una_columna = c("Luis","Hugo","José","Juan"),
                            color = c("Rojo","Verde","Verde","Rojo"))

# Eliminar la columna 2                            
nvo_dataframe <- nvo_dataframe[,-2]

# Eliminar las filas 2 y 3
nvo_dataframe <- nvo_dataframe[-c(2,3),]

