# prueba para MCMC
library(ggplot2)
library(Quandl)
library(reshape2)
library(timeSeries)
source("funciones.R")

# descarga y obtención de datos -------------------------------------------

# https://www.quandl.com/api/v3/datasets/CURRFX/USDMXN.csv?start_date=1996-03-26&end_date=2012-08-14
inter_t <- c(toString(as.Date(as.numeric(Sys.Date())-365)),  
             toString(as.Date(Sys.Date())))
ayer <- toString(as.Date(as.numeric(Sys.Date())-1))
url <- paste("https://www.quandl.com/api/v3/datasets/CURRFX/USDMXN.csv?start_date=",
             inter_t[1],"&end_date=",inter_t[2])
url2 <- paste("https://www.quandl.com/api/v3/datasets/BDM/SF43878.csv?start_date=",
              ayer,"&end_date=",ayer)
url3 <- paste("https://www.quandl.com/api/v3/datasets/FRBP/TBILL_MN.csv?start_date=",
              ayer,"&end_date=",ayer)
download.file(url,"USDMXN.csv")
download.file(url2,"tiie91.csv")
download.file(url3,"tbill91.csv")
tiie91 <- read.csv(file="tiie91.csv",header=TRUE,sep=",",na.strings=TRUE)
tbill91  <- read.csv(file="tbill91.csv",header=TRUE,sep=",",na.strings=TRUE)
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


deseos <- 100
y <- numeric()
for(i in 1:deseos){
  u <- runif(1)
  y[i] <- inversa(u, interv, freq_acum)
}

plot(1:deseos,y)
hist(y)


desv_est <- sd(rend$Value)

h <- (4*desv_est^5/(3*(n-1)))^(1/5)

x <- seq((v_min-desv_est),(v_max+desv_est),rango/10000)

fest <- numeric(length(x))
for(i in 1:(n-1)){
  fest <- fest + kernel(rend$Value[i], n, h, x)
}

plot(x,fest,type="l")
plot(density(rend$Value))

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


# Simulaciones  -----------------------------------------------------------
#Generación de simulaciones (trayectorias)
#El primer paso de las trayectorias depende del último dato real. Los demás dependen del simulado anterior.
days<-90  #días hasta el vencimiento o pago de proveedores :()

y_esti<-matrix(0, nrow=deseos, ncol=days)
y_esti[, 1] <- RDRL(rend$Value[n-1],deseos, x, fest, interv, freq_rel, freq_acum)
s_esti<-matrix(0, nrow=deseos, ncol=days)
s_esti[, 1] <- usdmxn$Rate[n]*exp(y_esti[, 1])
for(i in 1:deseos){
  for(j in 2:days){
    y_esti[i,j] <- mean(RDRL(y_esti[i,j-1],10, x, fest, interv, freq_rel, freq_acum))
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

#Gráfico de precio original + trayectorias
#Gráfico de precio original + trayectorias
nombres2 <- numeric()
nombres2[1] <- 'Precio_original'
nombres2[2] <- 'ID'

russo[, 2] <- c(1:nrow(russo))
for(i in 3:(ncol(russo))){
  nombres2[i] <- paste('Trayectoria', i-2)
}

colnames(russo) <- nombres2

russa <- melt(russo, id.vars = 'ID')
colnames(russa) <- c('ID', 'Valores', 'Valor')

v_esp<-colMeans(t(russo[,3:(ncol(s_estiff))]),na.rm = FALSE)
v_esp <- as.data.frame(v_esp)
v_esp[, 2] <- (1:nrow(russo))
colnames(v_esp) <- c('Promedios', 'ID')

ggplot()+
  geom_line(data = russa,aes(ID, Valor, color=Valores),size=0.05)+
  geom_line(data = v_esp, aes(x=ID, y=Promedios),color="dark blue", size=0.7)+
  geom_line(data=russo, aes(ID, Precio_original), color="dark orange")

#Media que no daba extraño

#Media y ver reduccion de varianza
#cOMO PROPONER UN PRECIO STRIKE
#Valuacion para tipo de cambio 
#Aumentar simulaciones


# Determinación de función de ganancias -----------------------------------
ST <- as.numeric(s_estiff[days+1, 2:(deseos+1)])
r <- tiie91$Value[1]/100
s0 <- s_estiff[1,2]
k  <- 15#s0*exp(r*days/252)
kf <- s0*exp(r*days/252)
sigma <- sd(rend$Value)*sqrt(252)
d1 <- (log(s0/k)+(r+sigma^2/2)*(days/252))/(sigma*sqrt(days/252))
d2 <- d1-sigma*sqrt(days/252)
Nd1<- pnorm(d1)
Nd2<- pnorm(d2)
ct_bs <- s0*Nd1-k*exp(-r*days/252)*Nd2 # unidades: pesos por dólar
ct_rdrl <- mean(pmax(ST - k,0)*exp(-r*days/252))
rd <- tiie91$Value[1]/100
rf <- tbill91$Value[1]/100
d1 <- (log(s0/k)+(rd-rf+sigma^2/2)*(days/252))/(sigma*sqrt(days/252))
d2 <- d1-sigma*sqrt(days/252)
Nd1<- pnorm(d1)
Nd2<- pnorm(d2)
ct_tc <- s0*exp(-rf*days/252)*Nd1-k*exp(-rd*days/252)*Nd2
#library(fOptions); GBSOption(TypeFlag = "c", S=s0, X=k, Time=days/252, r=r, b=r, sigma=sigma )
nocional <- 10000 # unidades (dólares que se quieren comprar)
ganan <- nocional * (pmax(ST - k,0) - ct*exp(r*days/252))
gananf<- nocional * (ST - kf)

# histograma de ganancias -------------------------------------------------
#hist(ganan)
ks.test(ganan, pnorm(deseos))

clases <- round(sqrt(deseos))
v_min <- min(ganan) 
v_max <- max(ganan)
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
  freq_acum[i] <- sum(ganan<interv[i])/(deseos)
}

FA <- cbind(interv,freq_acum)
plot(interv,freq_acum,type="l")

# Valor en riesgo
nc <- 0.99
VaR <- inversa(1-nc, interv, freq_acum)

# Prob. de ganar
prob_perder<-inversa(0, freq_acum, interv)
prob_ganar<-1-prob_perder


# kernel para ganancias ---------------------------------------------------

desv_est <- sd(ganan)

h <- (4*desv_est^5/(3*(deseos)))^(1/5)

x <- seq((v_min-desv_est),(v_max+desv_est),rango/10000)

fest <- numeric(length(x))
for(i in 1:(deseos)){
  fest <- fest + kernel(ganan[i], deseos+1, h, x)
}

plot(x,fest,type="l")
plot(density(ganan))

ganancia_esp <- fest%*%x/sum(fest) - nocional

# 
# # optimización ------------------------------------------------------------
# 
# ST <- as.numeric(s_estiff[days+1, 2:(deseos+1)])
# r <- tiie91$Value[1]/100
# s0 <- s_estiff[1,2]
# k  <- seq(0,20,0.0001)
# sigma <- sd(rend$Value)*sqrt(252)
# d1 <- (log(s0/k)+(r+sigma^2/2)*(days/252))/(sigma*sqrt(days/252))
# d2 <- d1-sigma*sqrt(days/252)
# Nd1 <- pnorm(d1)
# Nd2 <- pnorm(d2)
# ct <- s0*Nd1-k*exp(-r*days/252)*Nd2
# ganancia <- nocional * (mean(ST) - (k + ct))
# plot(k,ct,type="l")
# plot(k,(k + ct),type="l")
# plot(k,ganancia,type="l")

#Vamos comparando con el modelo de black scholes para el tipo de cambio. Este pedo es usando dle max(ST - k, 0) 
#donde ST es el calculado por nuestras simulaciones
#Comparamos también el resultado esperado de nuestras simulaciones con el de MexDer
#Conviene mejor experimentar y comparar que shiny. Perro Diablo.
