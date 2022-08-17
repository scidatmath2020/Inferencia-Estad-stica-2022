#### Los comentarios se hacen con ### (sharp o hash o gato)
# Este es un comentario 

#########################################################
##################### Tema: Listas ######################
#########################################################

lista_ejemplo  <- list(1,2,"3") #este es un ejemplo de lista

# Propiedad de nombres

mi_lista <- list(mi_vector = c(1,2,3),
                 mis_str = c("Hola a todos","Adiós"),
                 mi_booleano = TRUE)

mi_lista

#######

mi_lista$mi_vector

mi_lista$mi_booleano

mi_booelano

########

names(mi_lista)  # es el vector de nombres de los objetos de mi_lista

names(mi_lista)[c(1,3)]

names(mi_lista)[3] <- "mi_logico"

mi_lista

########

mi_vector <- c("Hola","a","todos","los","presentes")
mi_vector[3]

mi_vector[c(2,5)]


mi_lista[3]
mi_lista[[3]]


mi_lista[c(2,3)] <- c(3,4)

mi_lista[[2]][2] <- "bye bye"

mi_lista[2][2]

otra_lista <- list(c(1,23),FALSE,list("Hola","Adiós","Ya regresé"),"Scidata")
length(otra_lista)


otra_lista[[3]][[1]] <- "Un saludo"

otra_lista


class(c(1,3,4))

class(otra_lista)

class(otra_lista[[4]])
otra_lista
#####################################################
#####################################################
#####################################################

getwd()

setwd("C:/Users/hp master/OneDrive/Escritorio/mi_data")

dir()

mi_dataframe <- read.csv("online_retail2.csv")

View(mi_dataframe)

head(mi_dataframe,3)
tail(mi_dataframe,4)

names(mi_dataframe)[1] <- "facturacion"

names(mi_dataframe)

mi_dataframe$Country

row.names(mi_dataframe)

str(mi_dataframe)

nrow(mi_dataframe)
ncol(mi_dataframe)

dim(mi_dataframe)

dim(mi_dataframe)[1]
dim(mi_dataframe)[2]

###################

str(mi_dataframe)

mi_dataframe$facturacion <- as.numeric(mi_dataframe$facturacion)

str(mi_dataframe)







