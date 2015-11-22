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
usdmxn[,1:4] <- usdmxn[nrow(usdmxn):1,1:4]

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
  
  # esto es interpolación para sacar la inversa (o algo así). :)
  #sólo se tienen algunos puntos del histograma de frec. acumulado (límite), s
  #si cae dentro del intervalo no sabemos, para eso es interpolación
  #las y´s (num de deseos) representan valores aleatorios que se distribuyen como el hist de frec. normal.
  m <- (interv[nacho]-interv[nacho-1])/(freq_acum[nacho]-freq_acum[nacho-1])
  b <- interv[nacho-1]-m*freq_acum[nacho-1]
  y <- m*u+b
  return(y)
}

deseos <- 1000
y <- numeric()
for(i in 1:deseos){
  u <- runif(1)
  y[i] <- inversa(u)
}

plot(1:deseos,y)
hist(y)

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


# función objetivo y función propuesta  -----------------------------------

#Creación de función de probabilidad para la gráfica kernel.
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

#Creación del histograma de frecuencias y función de probabilidad con el hist.
freq_rel<-numeric(clases)
for (j in 1:(n-1)){
  for (i in 2:length(interv)){
    if (rend$Value[j]<interv[i] & rend$Value[j]>=interv[i-1]){
      freq_rel[i-1]<-freq_rel[i-1]+1
    }
  }
}
freq_rel<-freq_rel/sum(freq_rel)

#Función que arroja la probabilidad de la función por partes del hist.
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



# función general ---------------------------------------------------------


#Selección de nums aleatorios (y). Se eligen los que cumplen alpha, se adecúan más.
#Los que "pasen la prueba" serán la estimación para el siguiente día.
#Función RDRL. Generará aleatorios y discriminará.
RDRL <- function(e,deseos1){#d=un vector de aleatorios, e = ultimo dato
  y_esti<-numeric()
  equis <- runif(deseos1)
  for (i in 1:deseos1){
    y[i]<-inversa(runif(1))
    d<-y[i]
    #e<-rend$Value[n-1]
    alpha<-(f(d)*qu(e))/(f(e)*qu(d))
    y_esti[i]<-e+(d-e)*(equis[i]<alpha)
  }
  return(y_esti)
}


#Generación de simulaciones (trayectorias)
#El primer paso de las trayectorias depende del último dato real. Los demás dependen del simulado anterior.
days<-30  #días hasta el vencimiento o pago de proveedores :()
deseos<-20

y_esti<-matrix(0, nrow=deseos, ncol=days)
y_esti[, 1] <- RDRL(rend$Value[n-1],deseos)
s_esti<-matrix(0, nrow=deseos, ncol=days)
s_esti[, 1] <- usdmxn$Rate[n]*exp(y_esti[, 1])
for(i in 1:deseos){
  for(j in 2:days){
    y_esti[i,j] <- RDRL(y_esti[i,j-1],1)
    s_esti[i,j] <- s_esti[i, j-1]*exp(y_esti[i,j])
  }
}

s_esti <- as.data.frame(s_esti)

s_estif <- cbind(rep(usdmxn$Rate[n],deseos), s_esti)
s_estif <- as.data.frame(s_estif)

s_estiff <- cbind(1:nrow(t(s_estif)), t(s_estif))
s_estiff <- as.data.frame(s_estiff)
# gráficas ----------------------------------------------------------------


russo <- matrix(NA, nrow=(n+days), ncol = (deseos+2))
russo[1:n,1] <- usdmxn$Rate
russo[(n):(n+days),(3:ncol(russo))]<- t(s_estif)
russo <- as.data.frame(russo)

dd <- as.numeric(usdmxn$Date)
dd <- c(dd, (dd[n]+1):(dd[n]+days))
dd <- as.Date(dd)
dd<-as.data.frame(dd)

russo[, 2] <- dd

#Gráfico de solo las trayectorias
nombres <- numeric()
nombres[1] <- 'ID'

for(i in 2:ncol(s_estiff)){
  nombres[i] <- paste('Trayectoria', i-1)
}

colnames(s_estiff) <- nombres
russia <- melt(s_estiff, id.vars = "ID")
colnames(russia) <- c('ID', 'Trayectorias', 'Valor')
ggplot(russia,aes(ID,Valor,color=Trayectorias))+geom_line()
