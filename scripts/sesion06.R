setwd("C:/Users/hp master/OneDrive/Escritorio/mi_data")

mi_dataframe <- read.csv("online_retail2.csv")

names(mi_dataframe)

mean(abs(mi_dataframe$Quantity-mean(mi_dataframe$Quantity)))
mean(mi_dataframe$Quantity)

summary(mi_dataframe$Quantity)


sd(mi_dataframe$Quantity)
var(mi_dataframe$Quantity)

sd(mi_dataframe$Quantity)^2


mi_vec <- c(1,-1,4,23,7,8,-12,78)

sd(mi_vec)*sqrt((8-1)/8) #multiplicar por sqrt((n-1)/n)


##########################

cv <- sd(mi_dataframe$Price)/mean(mi_dataframe$Quantity)
cv


summary(mi_dataframe$Quantity)

