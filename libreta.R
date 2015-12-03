# prueba para MCMC
library(ggplot2)
library(Quandl)
library(reshape2)
library(timeSeries)
source("funciones.R")
source("datos.R")

rend$Value <- rend$Value - mean(rend$Value) + r/252

rend <- as.data.frame(rend)


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

FA <- as.data.frame(cbind(interv,freq_acum))
#plot(interv,freq_acum,type="l")
ggplot(data = FA, aes(x = interv, y = freq_acum)) + geom_line(size = 1, colour = "orange") + 
  labs(title = 'Frecuencia acumulada de rendimientos', y = 'Frecuencia', x = 'Intervalos')

y <- numeric()
for(i in 1:deseos){
  u <- runif(1)
  y[i] <- inversa(u, interv, freq_acum)
}

desv_est <- sd(rend$Value)

h <- (4*desv_est^5/(3*(n-1)))^(1/5)

x <- seq((v_min-desv_est),(v_max+desv_est),rango/10000)

fest <- numeric(length(x))
for(i in 1:(n-1)){
  fest <- fest + kernel(rend$Value[i], n, h, x)
}

#plot(x,fest,type="l")
wd <- as.data.frame(cbind(x,fest))

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

y_esti<-matrix(0, nrow=deseos, ncol=days)
y_esti[, 1] <- RDRL(rend$Value[n-1],deseos, x, fest, interv, freq_rel, freq_acum)

#Convirtiendo a precios los rendimientos estimados
s_esti<-matrix(0, nrow=deseos, ncol=days)
s_esti[, 1] <- usdmxn$Rate[n]*exp(y_esti[, 1])
for(i in 1:deseos){
  for(j in 2:days){
    y_esti[i,j] <- mean(RDRL(y_esti[i,j-1],1, x, fest, interv, freq_rel, freq_acum))
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


#Media que no daba extraño

#Media y ver reduccion de varianza
#cOMO PROPONER UN PRECIO STRIKE
#Valuacion para tipo de cambio 
#Aumentar simulaciones


# Determinación de función de ganancias -----------------------------------
ST <- as.numeric(s_estiff[days+1, 2:(deseos+1)])

s0 <- s_estiff[1,2]
k  <- 16.35 #s0*exp(r*days/252)
kf <- s0*exp(r*days/252)

rd <- tiie91$Value[1]/100
rf <- tbill91$X3.Mo[1]/100

#library(fOptions); GBSOption(TypeFlag = "c", S=s0, X=k, Time=days/252, r=r, b=r, sigma=sigma )
nocional <- 10000 # unidades (dólares que se quieren comprar)
#ganan <- nocional * (pmax(ST - k,0) - ct*exp(r*days/252))
ganan <- pmax(ST - k,0)*exp(-r/252*days)
gananf<- (ST - kf)*exp(-r*days/252)

# histograma de ganancias opcion -------------------------------------------------
#hist(ganan)

clases <- round(sqrt(deseos))
v_min <- min(ganan) 
v_max <- max(ganan)
rango <- v_max - v_min
delta <- rango / clases

#histograma de frec, acumulada opción
interv <- numeric()
interv[1] <- v_min
for(i in 2:(clases+1)){
  interv[i] <- interv[i-1] + delta
}

freq_acum <- numeric()
for(i in 1:length(interv)){
  freq_acum[i] <- sum(ganan<=interv[i])/(deseos)
}

#Creación del histograma de frecuencias y función de probabilidad con el hist.
freq_rel<-numeric(clases)
for (j in 1:deseos){
  for (i in 2:length(interv)){
    if (ganan[j]<=interv[i] & ganan[j]>interv[i-1]){
      freq_rel[i-1]<-freq_rel[i-1]+1
    }
  }
}
freq_rel<-freq_rel/sum(freq_rel)
# determinación del punto medio
punt_m <- (interv[2:(clases+1)]+interv[1:clases])/2
#plot(interv,freq_acum,type="l")
# qplot(ganan, geom = 'histogram', fill = I('dark green'),
#       color = I('white'),main = 'Histograma de ganancias', xlab = 'Ganancias', ylab = 'Frecuencia'
#       ,binwidth=delta)


ct_rdrl <- freq_rel %*% punt_m

