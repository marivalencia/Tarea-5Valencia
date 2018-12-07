Pregunta 2

library(quantmod)
library(tidyquant)
Apple <-tq_get("AAPL",get = "stock.prices",from = "2000-01-01", to = "2018-08-31", periodicity = "monthly")
Microsoft <-tq_get("MSFT",get = "stock.prices",from = "2000-01-01", to = "2018-08-31", periodicity = "monthly")

adjusted_vec <- Apple$adjusted
adj2a<- c(0,adjusted_vec[1:223])

calculo_retorno <- function(adjusted_vec,adj2a) {
  A <- adjusted_vec
  B <- adj2a
  reta <- (A-B)/B
  return(reta)
}
calculo_retorno(adjusted_vec,adj2a)  


skewe <- function(adjusted_vec){
  n1<- 1/length(adjusted_vec)
  up <- sum((adjusted_vec-mean(adjusted_vec))^3)
  down <-sum((adjusted_vec-mean(adjusted_vec))^2)
  s <- (n1*up)/((n1*down)^(3/2))
  return(s)
}
skewe(adjusted_vec)

kurto <- function(adjusted_vec){
  n1<- 1/length(adjusted_vec)
  up <- sum((adjusted_vec-mean(adjusted_vec))^4)
  down <-sum((adjusted_vec-mean(adjusted_vec))^2)
  k <- ((n1*up)/((n1*down)^2))
  return(k)
}
kurto(adjusted_vec)
kurtosis(adjusted_vec)

Jaq <- function(adjusted_vec){
  n <- length(adjusted_vec)
  s<- skewe(adjusted_vec)
  k<- kurto(adjusted_vec)
  
  JB <- n*(((s^3)/6)+((k-3)^2/24))
  return(JB)
}

Jaq(adjusted_vec)
qchisq(0.95,223)

testjb= function(adjusted_vec){
  if(Jaq(adjusted_vec) > qchisq(0.95,223) ) 
  {print(paste("Se distribuye normal"))}
  else 
  {print(paste("no se distribuye normal"))}
}
testjb(adjusted_vec)
##Para Microsoft

Microsoft <-tq_get("MSFT",get = "stock.prices",from = "2000-01-01", to = "2018-08-31", periodicity = "monthly")
adjusted_vecm <- Microsoft$adjusted
adj2m<- c(0,adjusted_vecm[1:223])

calculo_retorno <- function(adjusted_vecm,adj2m) {
  A <- adjusted_vecm
  B <- adj2m
  reta <- (A-B)/B
  return(reta)
}
calculo_retorno(adjusted_vecm,adj2m)  


skewe <- function(adjusted_vecm){
  n1<- 1/length(adjusted_vecm)
  up <- sum((adjusted_vecm-mean(adjusted_vecm))^3)
  down <-sum((adjusted_vecm-mean(adjusted_vecm))^2)
  s <- (n1*up)/((n1*down)^(3/2))
  return(s)
}
skewe(adjusted_vecm)

kurto <- function(adjusted_vecm){
  n1<- 1/length(adjusted_vecm)
  up <- sum((adjusted_vecm-mean(adjusted_vecm))^4)
  down <-sum((adjusted_vecm-mean(adjusted_vecm))^2)
  k <- ((n1*up)/((n1*down)^2))
  return(k)
}
kurto(adjusted_vecm)


Jaq <- function(adjusted_vecm){
  n <- length(adjusted_vecm)
  s<- skewe(adjusted_vecm)
  k<- kurto(adjusted_vecm)
  
  JB <- n*(((s^3)/6)+((k-3)^2/24))
  return(JB)
}

Jaq(adjusted_vecm)


testjb= function(adjusted_vecm){
  if(Jaq(adjusted_vecm) > qchisq(0.95,length(adjusted_vecm)) ) 
  {print(paste("Se distribuye normal"))}
  else 
  {print(paste("no se distribuye normal"))}
}
testjb(adjusted_vecm)