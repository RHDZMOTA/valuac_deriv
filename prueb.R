# prueba para MCMC

library(Quandl)
library(ggplot2)


# descarga y obtenci�n de datos -------------------------------------------

# https://www.quandl.com/api/v3/datasets/CURRFX/USDMXN.csv?start_date=1996-03-26&end_date=2012-08-14
inter_t <- c(toString(as.Date(as.numeric(Sys.Date())-365)),  
             toString(as.Date(Sys.Date())))

url <- paste("https://www.quandl.com/api/v3/datasets/CURRFX/USDMXN.csv?start_date=",
             inter_t[1],"&end_date=",inter_t[2])
download.file(url,"USDMXN.csv")
usdmxn <-  read.csv(file="USDMXN.csv",header=TRUE,sep=",",na.strings=TRUE)
usdmxn$Date <- as.Date(usdmxn$Date)

ggplot(data=usdmxn, aes(x=Date, y=Rate))+geom_line()

# rendimientos logar�tmicos
n <- nrow(usdmxn) 
rend <- as.data.frame(usdmxn)
rend$Value[2:n] <- log(usdmxn[2:n,2]/usdmxn[1:(n-1),2])
rend <- rend[2:n,]

ggplot(data=rend, aes(x=Date, y=Value))+geom_line()


# determinaci�n de q ------------------------------------------------------
# determinaci�n del histograma de frecuencias para rend. log

hist(rend$Value)
ks.test(rend$Value, pnorm(n-1))

clases <- round(sqrt(n-1))
v_min <- min(rend$Value) 
v_max <- max(rend$Value)
rango <- v_max - v_min
delta <- rango / clases
 
#histograma de frec, acumulada
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
  
  # esto es interpolaci�n para sacar la inversa (o algo as�). :)
  #s�lo se tienen algunos puntos del histograma de frec. acumulado (l�mite), s
  #si cae dentro del intervalo no sabemos, para eso es interpolaci�n
  #las y�s (num de deseos) representan valores aleatorios que se distribuyen como el hist de frec. normal.
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

# calcular la funci�n objetivo (kernel) -----------------------------------
# Variables:
# h : par�metro para kernel (quien sabe)
# x : valores del "eje equis"
# kero: funci�n que calcula la distr. de densidad para cada dato con un
# kernel gaussiano
# fest: es la funci�n de densidad de los rendimientos (sumatoria de la func.
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

#Creaci�n de funci�n de probabilidad para la gr�fica kernel.
f<-function(u2){
 nacho2<-1
 if (u2>=max(x) | u2<=min(x))
   prob<-0.00000000000001
 else {
   while(u2>=x[nacho2])
     nacho2<-nacho2+1
   m <- (fest[nacho2]-fest[nacho2-1])/(x[nacho2]-x[nacho2-1])
   b <- fest[nacho2-1]-m*x[nacho2-1]
   y <- m*u2+b
   prob<-y/sum(fest)
 }
  return(prob)
}

#Creaci�n del histograma de frecuencias y funci�n de probabilidad con el hist.
freq_rel<-numeric(clases)
for (j in 1:(n-1)){
  for (i in 2:length(interv)){
    if (rend$Value[j]<interv[i] & rend$Value[j]>=interv[i-1]){
      freq_rel[i-1]<-freq_rel[i-1]+1
    }
  }
}
freq_rel<-freq_rel/sum(freq_rel)

qu<-function(u3){
  nacho3<-1
  if (u3>=max(interv) | u3<=min(interv))
    prob<-0.000000000000000001
  else {
    while(u3>=interv[nacho3])
      nacho3<-nacho3+1
#       if(nacho3 >= length(interv))
#         nacho3 <- length(interv)
    prob<-freq_rel[nacho3-1]#/sum(datoshrv)
  }
  return(prob)
}

#Selecci�n de nums aleatorios (y). Se eligen los que cumplen alpha, se adec�an m�s.
#Los que "pasen la prueba" ser�n la estimaci�n para el siguiente d�a.

estimadas <- function(e){#d=un vector de aleatorios, e = ultimo dato
y_esti<-numeric()
equis <- runif(deseos)
for (i in 1:deseos){
  d<-y[i]
  e<-rend$Value[n-1]
  alpha<-(f(d)*qu(e))/(f(e)*qu(d))
  y_esti[i]<-e+(d-e)*(equis[i]<alpha)
  
}
}
# ce <- numeric()
# a <- 0
# for(i in 1:length(y_esti)){
#   if(is.na(y_esti[i])==T){
#     a <- a+1
#     ce[a]  <- i
#   }
# }

#Funci�n RDRL. Generar� aleatorios y discriminar�.




