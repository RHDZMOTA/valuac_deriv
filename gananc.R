# ganancias
loque2 <- function(s_estiff, k, days){
# Determinación de función de ganancias -----------------------------------
ST <- as.numeric(s_estiff[days+1, 2:(deseos+1)])

s0 <- s_estiff[1,2]
kf <- s0*exp(r*days/252)
sigma <- sd(rend$Value)*sqrt(252)
d1 <- (log(s0/k)+(r+sigma^2/2)*(days/252))/(sigma*sqrt(days/252))
d2 <- d1-sigma*sqrt(days/252)
Nd1<- pnorm(d1)
Nd2<- pnorm(d2)
ct_bs <- s0*Nd1-k*exp(-r*days/252)*Nd2 # unidades: pesos por dólar
ct_rdrl <- mean(pmax(ST - k,0)*exp(-r*days/252))
d1 <- (log(s0/k)+(rd-rf+sigma^2/2)*(days/252))/(sigma*sqrt(days/252))
d2 <- d1-sigma*sqrt(days/252)
Nd1<- pnorm(d1)
Nd2<- pnorm(d2)
ct_tc <- s0*exp(-rf*days/252)*Nd1-k*exp(-rd*days/252)*Nd2
ct <- max(ct_tc,ct_bs,ct_rdrl)
#library(fOptions); GBSOption(TypeFlag = "c", S=s0, X=k, Time=days/252, r=r, b=r, sigma=sigma )
nocional <- 10000 # unidades (dólares que se quieren comprar)
#ganan <- nocional * (pmax(ST - k,0) - ct*exp(r*days/252))
ganan <- nocional * (pmax(ST - k,0)*exp(-r/252*days) - ct)
gananf<- nocional * (ST - kf)*exp(-r*days/252)

# histograma de ganancias opcion -------------------------------------------------
#hist(ganan)
normita <- ks.test(ganan, pnorm(deseos))

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
  freq_acum[i] <- sum(ganan<interv[i])/(deseos)
}

FA <- as.data.frame(cbind(interv,freq_acum))

# Valor en riesgo opción
nc <- 0.99
VaR <- inversa(1-nc, interv, freq_acum)

# Prob. de ganar opción
prob_perder <- inversa(0, freq_acum, interv)
prob_ganar <- 1-prob_perder

# kernel para ganancias opción ---------------------------------------------------

desv_est <- sd(ganan)

h <- (4*desv_est^5/(3*(deseos)))^(1/5)

x <- seq((v_min-desv_est),(v_max+desv_est),rango/10000)

fest <- numeric(length(x))
for(i in 1:(deseos)){
  fest <- fest + kernel(ganan[i], deseos+1, h, x)
}


wd <- as.data.frame(cbind(x, fest))

ganancia_esp <- fest%*%x/sum(fest) - nocional

# 

# Histograma de ganancias Futuro ------------------------------------------

normita2 <- ks.test(gananf, pnorm(deseos))

clases2 <- round(sqrt(deseos))
v_min2 <- min(gananf) 
v_max2 <- max(gananf)
rango2 <- v_max2 - v_min2
delta2 <- rango2 / clases2

#histograma de frec, acumulada opción
interv <- numeric()
interv[1] <- v_min2
for(i in 2:(clases2+1)){
  interv[i] <- interv[i-1] + delta
}

freq_acum <- numeric()
for(i in 1:length(interv)){
  freq_acum[i] <- sum(gananf<interv[i])/(deseos)
}

FA2 <- as.data.frame(cbind(interv,freq_acum))

# Valor en riesgo opción
nc <- 0.99
VaRf <- inversa(1-nc, interv, freq_acum)

# Prob. de ganar opción
prob_perderf <- inversa(0, freq_acum, interv)
prob_ganarf <- 1-prob_perderf

# Kernel ganancia futuro --------------------------------------------------

desv_est2 <- sd(gananf)

h <- (4*desv_est2^5/(3*(deseos)))^(1/5)

x <- seq((v_min2-desv_est2),(v_max2+desv_est2),rango2/10000)

festf <- numeric(length(x))
for(i in 1:(deseos)){
  festf <- festf + kernel(gananf[i], deseos+1, h, x)
}

wd2 <- as.data.frame(cbind(x, festf))

ganancia_espf <- festf%*%x/sum(festf) - nocional
resultados2 <- list()
resultados2[[1]] <- kf
resultados2[[2]] <- ST
resultados2[[3]] <- as.data.frame(cbind(ST, ganan))
resultados2[[4]] <- as.data.frame(cbind(ST, gananf))

names(resultados2) <- c('kf', 'ST', 'Ganan_opcion', 'Ganan_forward')
return(resultados2)
}
