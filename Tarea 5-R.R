<<<<<<< HEAD
#Pregunta 2


library(tidyquant)
Apple <-tq_get("AAPL",get = "stock.prices",from = "2000-01-01", to = "2018-08-31", periodicity = "monthly")
=======
Pregunta 2

library(quantmod)
library(tidyquant)
Apple <-tq_get("AAPL",get = "stock.prices",from = "2000-01-01", to = "2018-08-31", periodicity = "monthly")
Microsoft <-tq_get("MSFT",get = "stock.prices",from = "2000-01-01", to = "2018-08-31", periodicity = "monthly")

>>>>>>> 4186f3ea41b10d10ffc4a5d7e2704c89c85cc466
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
<<<<<<< HEAD
=======
kurtosis(adjusted_vec)
>>>>>>> 4186f3ea41b10d10ffc4a5d7e2704c89c85cc466

Jaq <- function(adjusted_vec){
  n <- length(adjusted_vec)
  s<- skewe(adjusted_vec)
  k<- kurto(adjusted_vec)
<<<<<<< HEAD
  JB <- n*(((s^3)/6)+((k-3)^2/24))
  return(JB)
}
Jaq(adjusted_vec)

=======
  
  JB <- n*(((s^3)/6)+((k-3)^2/24))
  return(JB)
}

Jaq(adjusted_vec)
qchisq(0.95,223)
>>>>>>> 4186f3ea41b10d10ffc4a5d7e2704c89c85cc466

testjb= function(adjusted_vec){
  if(Jaq(adjusted_vec) > qchisq(0.95,223) ) 
  {print(paste("Se distribuye normal"))}
  else 
  {print(paste("no se distribuye normal"))}
}
testjb(adjusted_vec)
<<<<<<< HEAD

=======
>>>>>>> 4186f3ea41b10d10ffc4a5d7e2704c89c85cc466
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
<<<<<<< HEAD
=======
  
>>>>>>> 4186f3ea41b10d10ffc4a5d7e2704c89c85cc466
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
<<<<<<< HEAD
testjb(adjusted_vecm)

###Pregunta3

set.seed(123)

reps = 10000
betas = matrix(NA, nrow = reps, ncol = 16)

bet0 = 2
bet1 = 2.5
bet2 = 1

su = 1

n = c(50, 100, 500, 1000) 

for (j in 1:length(n)) {
  
  x1=rnorm(n[j],20,1)
  x2=(0.8*x1) + rnorm(n[j],0,1)
  
  for (i in 1:reps) {
    
    u= rnorm(n[j],0,su)
    v = bet2*x2+u
    
    ym = bet0 + bet1*x1 + v 
    yb = bet0 +bet1*x1 + bet2*x2 + u
    
    modelo_m = lm(ym~x1)
    modelo_b = lm(yb~x1+x2)
    
    betas[i,j] = modelo_m$coef[1]
    betas[i,j+4] = modelo_m$coef[2]
    
    betas[i,j+8] = modelo_b$coef[1]
    betas[i,j+12] = modelo_b$coef[2] }}

bet_data <- data.frame(betas)

apply(bet_data, 2, mean)
apply(bet_data, 2, var)

##Pregunta 3.b

ggplot(bet_data) + geom_histogram(aes(bet_data[,5], y=..density..), col="green", bins = 30) +
  stat_function(fun=dnorm, args=list(mean=mean(bet_data[,5]), sd=sd(bet_data[,5])), 
                geom="line", colour="orange", size=1.5) + ylab("Densidad") +   ggtitle("n=50 modelo_m") + xlab(expression(hat(beta)[1])) + theme_bw()


ggplot(bet_data) +  geom_histogram(aes(bet_data[,6], y=..density..), col="green", bins = 30) +
  stat_function(fun=dnorm, args=list(mean=mean(bet_data[,6]), sd=sd(bet_data[,6])), geom="line", colour="orange", size=1.5) +
  ylab("Densidad") +   ggtitle("n=100 modelo_m") + xlab(expression(hat(beta)[1])) + theme_bw()

ggplot(bet_data) + geom_histogram(aes(bet_data[,7], y=..density..), col="green", bins = 30) +
  stat_function(fun=dnorm, args=list(mean=mean(bet_data[,7]), sd=sd(bet_data[,7])), 
                geom="line", colour="orange", size=1.5) +
  ylab("Densidad") +   ggtitle("n=500 modelo_m") + xlab(expression(hat(beta)[1])) + theme_bw()

ggplot(bet_data) + geom_histogram(aes(bet_data[,8], y=..density..), col="green", bins = 30) +
  stat_function(fun=dnorm, args=list(mean=mean(bet_data[,8]), sd=sd(bet_data[,8])), 
                geom="line", colour="orange", size=1.5) + ylab("Densidad") +   
  ggtitle("n=1000 modelo_m") + xlab(expression(hat(beta)[1])) + theme_bw()

ggplot(bet_data) + geom_histogram(aes(bet_data[,13], y=..density..), col="black", bins = 30) +
  stat_function(fun=dnorm, args=list(mean=mean(bet_data[,13]), sd=sd(bet_data[,13])), 
                geom="line", colour="orange", size=1.5) + ylab("Densidad") +   
  ggtitle("n=50, modelo_b") + xlab(expression(hat(beta)[1])) + theme_bw()

ggplot(bet_data) + geom_histogram(aes(bet_data[,14], y=..density..), col="black", bins = 30) +
  stat_function(fun=dnorm, args=list(mean=mean(bet_data[,14]), sd=sd(bet_data[,14])), 
                geom="line", colour="orange", size=1.5) + ylab("Densidad") +   
  ggtitle("n=100 modelo_b") + xlab(expression(hat(beta)[1])) + theme_bw()

ggplot(bet_data) + geom_histogram(aes(bet_data[,15], y=..density..), col="black", bins = 30) +
  stat_function(fun=dnorm, args=list(mean=mean(bet_data[,15]), sd=sd(bet_data[,15])), 
                geom="line", colour="orange", size=1.5) + ylab("Densidad") +   
  ggtitle("n=500 modelo_b") + xlab(expression(hat(beta)[1])) + theme_bw()

ggplot(bet_data) + geom_histogram(aes(bet_data[,16], y=..density..), col="black", bins = 30) +
  stat_function(fun=dnorm, args=list(mean=mean(bet_data[,16]), sd=sd(bet_data[,16])), 
                geom="line", colour="orange", size=1.5) + ylab("Densidad") +   
  ggtitle("n=1000 modelo_b") + xlab(expression(hat(beta)[1])) + theme_bw()

##Pregunta 3c

betasc = matrix(NA, nrow = reps, ncol = 16)

bet0 = 2
bet1 = 2.5
bet2 = 1

su = 1

n = c(50, 100, 500, 1000) 

for (j in 1:length(n)) {
  
  x1=rnorm(n[j],20,1)
  x2c=runif(n[j],0,1)
  
  for (i in 1:reps) {
    
    u= rnorm(n[j],0,su)
    v = bet2*x2c+u
    
    ym = bet0 + bet1*x1 + v 
    yb = bet0 +bet1*x1 + bet2*x2c + u
    
    modelo_m = lm(ym~x1)
    modelo_b = lm(yb~x1+x2c)
    
    betas[i,j] = modelo_m$coef[1]
    betas[i,j+4] = modelo_m$coef[2]
    
    betas[i,j+8] = modelo_b$coef[1]
    betas[i,j+12] = modelo_b$coef[2] }}

betc_data <- data.frame(betas)

apply(betc_data, 2, mean)
apply(betc_data, 2, var)

ggplot(betc_data) + geom_histogram(aes(betc_data[,5], y=..density..), col="green", bins = 30) +
  stat_function(fun=dnorm, args=list(mean=mean(betc_data[,5]), sd=sd(betc_data[,5])), 
                geom="line", colour="orange", size=1.5) + ylab("Densidad") +   ggtitle("n=50 modelo_m") + xlab(expression(hat(beta)[1])) + theme_bw()


ggplot(betc_data) +  geom_histogram(aes(betc_data[,6], y=..density..), col="green", bins = 30) +
  stat_function(fun=dnorm, args=list(mean=mean(betc_data[,6]), sd=sd(betc_data[,6])), geom="line", colour="orange", size=1.5) +
  ylab("Densidad") +   ggtitle("n=100 modelo_m") + xlab(expression(hat(beta)[1])) + theme_bw()

ggplot(betc_data) + geom_histogram(aes(betc_data[,7], y=..density..), col="green", bins = 30) +
  stat_function(fun=dnorm, args=list(mean=mean(betc_data[,7]), sd=sd(betc_data[,7])), 
                geom="line", colour="orange", size=1.5) +
  ylab("Densidad") +   ggtitle("n=500 modelo_m") + xlab(expression(hat(beta)[1])) + theme_bw()

ggplot(betc_data) + geom_histogram(aes(betc_data[,8], y=..density..), col="green", bins = 30) +
  stat_function(fun=dnorm, args=list(mean=mean(betc_data[,8]), sd=sd(betc_data[,8])), 
                geom="line", colour="orange", size=1.5) + ylab("Densidad") +   
  ggtitle("n=1000 modelo_m") + xlab(expression(hat(beta)[1])) + theme_bw()

ggplot(betc_data) + geom_histogram(aes(betc_data[,13], y=..density..), col="black", bins = 30) +
  stat_function(fun=dnorm, args=list(mean=mean(betc_data[,13]), sd=sd(betc_data[,13])), 
                geom="line", colour="orange", size=1.5) + ylab("Densidad") +   
  ggtitle("n=50, modelo_b") + xlab(expression(hat(beta)[1])) + theme_bw()

ggplot(betc_data) + geom_histogram(aes(betc_data[,14], y=..density..), col="black", bins = 30) +
  stat_function(fun=dnorm, args=list(mean=mean(betc_data[,14]), sd=sd(betc_data[,14])), 
                geom="line", colour="orange", size=1.5) + ylab("Densidad") +   
  ggtitle("n=100 modelo_b") + xlab(expression(hat(beta)[1])) + theme_bw()

ggplot(betc_data) + geom_histogram(aes(betc_data[,15], y=..density..), col="black", bins = 30) +
  stat_function(fun=dnorm, args=list(mean=mean(betc_data[,15]), sd=sd(betc_data[,15])), 
                geom="line", colour="orange", size=1.5) + ylab("Densidad") +   
  ggtitle("n=500 modelo_b") + xlab(expression(hat(beta)[1])) + theme_bw()

ggplot(betc_data) + geom_histogram(aes(betc_data[,16], y=..density..), col="black", bins = 30) +
  stat_function(fun=dnorm, args=list(mean=mean(betc_data[,16]), sd=sd(betc_data[,16])), 
                geom="line", colour="orange", size=1.5) + ylab("Densidad") +   
  ggtitle("n=1000 modelo_b") + xlab(expression(hat(beta)[1])) + theme_bw()
=======
testjb(adjusted_vecm)
>>>>>>> 4186f3ea41b10d10ffc4a5d7e2704c89c85cc466
