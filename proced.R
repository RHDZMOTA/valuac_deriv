# determinación de procedimiento para simular las trayectorias del tipo de cambio
library(reshape2)
source("funciones.R")

# determinación de q ------------------------------------------------------
# determinación del histograma de frecuencias para rend. log

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


deseos <- 100
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



loque <- function(days){
  y_esti<-matrix(0, nrow=deseos, ncol=days)
  y_esti[, 1] <- RDRL(rend$Value[n-1],deseos, x, fest, interv, freq_rel, freq_acum)
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
  resultados <- list()
  resultados[[1]] <- russa
  resultados[[2]] <- v_esp
  resultados[[3]] <- russo
  resultados[[4]] <- s_estiff
  return(resultados)
}

