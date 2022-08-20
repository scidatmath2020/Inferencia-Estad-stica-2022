setwd("C:/Users/hp master/OneDrive/Escritorio/mi_data")

mi_dataframe <- read.csv("online_retail2.csv")
View(mi_dataframe)


names(mi_dataframe)

mean(mi_dataframe$Price)

### letters es un vector que ya viene cargado en RStudio, formado por
### el abecedario en inglés y minúsculas

otro_data <- data.frame(col1=c(1,5,2,8,9,NA,15,0),col2=c(2001:2007,0),col3=letters[1:8])

mean(otro_data$col2[otro_data$col2 != 0])

20000000 + NA

mean(otro_data$col1, na.rm = TRUE)

mean(otro_data$col3)

colMeans(otro_data[,1:2],na.rm=TRUE)

mean(mi_dataframe$Price)

otro_data[otro_data$col1 == 0,] <- c(NA,NA,NA)

otro_data

datos <- otro_data$col1[!is.na(otro_data$col1)]

median(datos)

median(mi_dataframe$Price,na.rm = TRUE)

##############################

sort(c(2, 5 ,89, -2, 4, 0, -4, 9))

##############################


median(mi_dataframe$Price)

table(mi_dataframe$Price <= 2.1)

##############################

summary(mi_dataframe$Price)

quantile(mi_dataframe$Price)

?quantile

## seq(a,b,c) toma el intervalo [a,b] y lo divide en piezas de longitud c
quantile(mi_dataframe$Price,probs = seq(0,1,0.01))

#################################

sort(table(mi_dataframe$Country),decreasing = TRUE)[1]

sort(table(c("a","a","b","b","a","c","b")),decreasing = TRUE)[1]

which(table(c("a","a","b","b","a","c","b"))==max(table(c("a","a","b","b","a","c","b"))))

#### En la biblioteca psych hay una función llamada describe.by() 
#################################
### stats

#### 
install.packages("modeest")
library(modeest)

mfv(c("a","a","b","b","a","c","b"))

#########################################

mean(mi_dataframe$Price)

#########################################

max(mi_dataframe$Price) - min(mi_dataframe$Price)
IQR(mi_dataframe$Price) #rango intercuartílico
median(mi_dataframe$Price)



otro_data