SIMULADOR <- function(){
  simulacion_llegada <- cumsum(rexp(100,rate = 0.1))
  simulacion_llegada <- simulacion_llegada[simulacion_llegada<600]
  simulacion_clientes <- sample(c(1,2,3,4),
                                size = length(simulacion_llegada),
                                replace = TRUE,
                                prob = c(0.3,0.5,0.1,0.1))
  simulacion <- data.frame(simulacion_llegada,simulacion_clientes)
  simulacion$gasto_total <- sapply(simulacion_clientes,function(x){sum(rnorm(x,mean=35,sd=5))})
  return(sum(simulacion$gasto_total))
}

SIMULADOR()