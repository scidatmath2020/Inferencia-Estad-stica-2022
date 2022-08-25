setwd("C:/Users/hp master/OneDrive/Escritorio/mi_data")
library(ggplot2)

cifra_negra <- read.csv("cifra_negra.csv")

ggplot(data = cifra_negra) +
  geom_point(aes(x = delitos,y=porcentaje,color = as.factor(anio),alpha=0.1))

anio_2020 <- cifra_negra[cifra_negra$anio == 2020,]

ggplot(data = anio_2020,aes(x = delitos,
                            y=porcentaje,
                            label = Entidad,
                            size = delitos,
                            color = zona
                            #shape = zona 
                            )) +
geom_point(alpha=0.5) +
geom_text(aes(hjust = 0, vjust = -1),size = 5,alpha = 0.5) +
guides(alpha = "none")
  

anio_2020_incompleto <- anio_2020[anio_2020$Entidad != "Ciudad de México" & anio_2020$Entidad != "Estado de México",]  
  
ggplot(data = anio_2020_incompleto,aes(x = delitos,
                            y=cifra_negra,
                            label = Entidad,
                            size = cifra_negra,
                            color = zona
                            #shape = zona 
)) +
  geom_point(alpha=0.5) +
  geom_text(aes(hjust = 0, vjust = -1),size = 5,alpha = 0.5) +
  guides(alpha = "none")
