# prueba para MCMC

library(Quandl)
library(ggplot2)


# descarga y obtención de datos -------------------------------------------

# https://www.quandl.com/api/v3/datasets/CURRFX/USDMXN.csv?start_date=1996-03-26&end_date=2012-08-14
inter_t <- c(toString(as.Date(as.numeric(Sys.Date())-365)),  
             toString(as.Date(Sys.Date())))
url <- paste("https://www.quandl.com/api/v3/datasets/CURRFX/USDMXN.csv?start_date=",
             inter_t[1],"&end_date=",inter_t[2])
download.file(url,"USDMXN.csv")
usdmxn <-  read.csv(file="USDMXN.csv",header=TRUE,sep=",",na.strings=TRUE)
usdmxn$Date <- as.Date(usdmxn$Date)

ggplot(data=usdmxn, aes(x=Date, y=Rate))+geom_line()

# rendimientos logarítmicos
n <- nrow(usdmxn) 
rend <- as.data.frame(usdmxn)
rend$Value[2:n] <- log(usdmxn[2:n,2]/usdmxn[1:(n-1),2])
rend <- rend[2:n,]

ggplot(data=rend, aes(x=Date, y=Value))+geom_line()


# determinación de q ------------------------------------------------------
# determinación del histograma de frecuencias para rend. log

hist(rend$Value)
ks.test(rend$Value, pnorm(n-1))

clases <- round(sqrt(n-1))
v_min <- min(rend$Value) 
v_max <- max(rend$Value)
rango <- v_max - v_min
delta <- rango / clases

interv <- numeric()
interv[1] <- v_min
for(i in 2:(clases+1)){
  interv[i] <- interv[i-1] + delta
}

freq_acum <- numeric()
for(i in 1:length(interv)){
  freq_acum[i] <- sum(rend$Value<interv[i])/(n-1)
}

FA <- cbind(interv,freq_acum)
plot(interv,freq_acum,type="l")


inversa <- function(u){
  nacho <- 1
  
  while(u>freq_acum[nacho]){
    nacho <- nacho + 1
  }
  
  # esto es interpolación para sacar la inversa (o algo así). :)
  m <- (interv[nacho]-interv[nacho-1])/(freq_acum[nacho]-freq_acum[nacho-1])
  b <- interv[nacho-1]-m*freq_acum[nacho-1]
  y <- m*u+b
  return(y)
}

deseos <- 10000
y <- numeric()
for(i in 1:deseos){
  u <- runif(1)
  y[i] <- inversa(u)
}


plot(1:deseos,y)
hist(y)
hist(rend$Value, nclass=clases)


# calcular la función objetivo (kernel) -----------------------------------
# Variables:
# h : parámetro para kernel (quien sabe)
# x : valores del "eje equis"
# kero: función que calcula la distr. de densidad para cada dato con un
# kernel gaussiano
# fest: es la función de densidad de los rendimientos (sumatoria de la func.
# de densidad para cada dato obtenido por kero).

desv_est <- sd(rend$Value)

h <- (4*desv_est^5/(3*(n-1)))^(1/5)

x <- seq((v_min-desv_est),(v_max+desv_est),rango/10000)
kernel <- function(equis){
  kero <- exp(-(x-equis)^2/(2*h^2))/(sqrt(2*pi)*(n-1)*h)
  return(kero)
}

fest <- numeric(length(x))
for(i in 1:(n-1)){
  fest <- fest + kernel(rend$Value[i])
}


plot(x,fest,type="l")
#plot(density(rend$Value))


