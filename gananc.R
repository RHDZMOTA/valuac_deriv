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
  rd <- tiie91$Value[1]/100
  rf <- tbill91$X3.Mo[1]/100
  d1 <- (log(s0/k)+(rd-rf+sigma^2/2)*(days/252))/(sigma*sqrt(days/252))
  d2 <- d1-sigma*sqrt(days/252)
  Nd1<- pnorm(d1)
  Nd2<- pnorm(d2)
  ct_tc <- s0*exp(-rf*days/252)*Nd1-k*exp(-rd*days/252)*Nd2
  ct <- ct_tc
  #library(fOptions); GBSOption(TypeFlag = "c", S=s0, X=k, Time=days/252, r=r, b=r, sigma=sigma )
  nocional <- 10000 # unidades (dólares que se quieren comprar)
  #ganan <- nocional * (pmax(ST - k,0) - ct*exp(r*days/252))
  ganan <- pmax(ST - k,0)*exp(-r/252*days)
  gananf<- (ST - kf)*exp(-r*days/252)
  
  # histograma de ganancias opcion -------------------------------------------------

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
  ct_rdrl <- freq_rel %*% punt_m
  # probabilidad de ganar
  prob_ejerce <-sum(ganan>0)/deseos # probabilidad de ganar, osea ejercer. Obvi. :()
  prob_perde <- 1 - prob_ejerce # solamente se "pierde" la prima 
  
  # Histograma de ganancias Futuro ------------------------------------------
  #hist(ganan)

  clases2 <- round(sqrt(deseos))
  v_min2 <- min(gananf) 
  v_max2 <- max(gananf)
  rango2 <- v_max2 - v_min2
  delta2 <- rango2 / clases2
  
  #histograma de frec, acumulada opción
  intervf <- numeric()
  intervf[1] <- v_min2
  for(i in 2:(clases2+1)){
    intervf[i] <- intervf[i-1] + delta2
  }
  
  freq_acumf <- numeric()
  for(i in 1:length(intervf)){
    freq_acumf[i] <- sum(gananf<=intervf[i])/(deseos)
  }
  

  #Creación del histograma de frecuencias y función de probabilidad con el hist.
  freq_relf<-numeric(clases2)
  for (j in 1:deseos){
    for (i in 2:length(intervf)){
      if (gananf[j]<=intervf[i] & gananf[j]>intervf[i-1]){
        freq_relf[i-1]<-freq_relf[i-1]+1
      }
    }
  }
  freq_relf<-freq_relf/sum(freq_relf)
  # determinación del punto medio
  punt_mf <- (intervf[2:(clases2+1)]+intervf[1:clases2])/2
  
  gananf_neta <- freq_relf %*% punt_mf
  # probabilidad de ganar
  prob_ganarf<-sum(gananf>0)/deseos # probabilidad de ganar, osea ejercer. Obvi. :()
  prob_perdef <- 1 - prob_ganarf # solamente se "pierde" la prima 
  
  
  # ganancia neta de la opción call -----------------------------------------
  ganan_nets <- ganan - ct_tc
  
  clasesn <- round(sqrt(deseos))
  v_minn <- min(ganan_nets) 
  v_maxn <- max(ganan_nets)
  rangon <- v_maxn - v_minn
  deltan <- rangon / clasesn
  
  #histograma de frec, acumulada opción
  intervn <- numeric()
  intervn[1] <- v_minn
  for(i in 2:(clasesn+1)){
    intervn[i] <- intervn[i-1] + deltan
  }
  
  freq_acumn <- numeric()
  for(i in 1:length(intervn)){
    freq_acumn[i] <- sum(ganan_nets<=intervn[i])/(deseos)
  }
  
  
  
  #Creación del histograma de frecuencias y función de probabilidad con el hist.
  freq_relfn<-numeric(clasesn)
  for (j in 1:deseos){
    for (i in 2:length(intervn)){
      if (ganan_nets[j]<=intervn[i] & ganan_nets[j]>intervn[i-1]){
        freq_relfn[i-1]<-freq_relfn[i-1]+1
      }
    }
  }
  freq_relfn<-freq_relfn/sum(freq_relfn)
  # determinación del punto medio
  punt_mn <- (intervn[2:(clasesn+1)]+intervn[1:clasesn])/2

  ganan_neta <- freq_relfn %*% punt_mn
  # probabilidad de ganar
  prob_ganarn<-sum(ganan_nets>0)/deseos # probabilidad de ganar, osea ejercer. Obvi. :()
  prob_perden <- 1 - prob_ganarn # solamente se "pierde" la prima 
  
  
  result <- c(ct_rdrl,prob_ejerce,ct_tc,ganan_neta,prob_ganarn,gananf_neta,prob_ganarf)
  names(result) <- c("ct_rdrl","prob_ejerce","ct_tc","ganan_neta","prob_ganarn","gananf_neta","prob_ganarf")
  
resultados2 <- list()
resultados2[[1]] <- kf
resultados2[[2]] <- ST
resultados2[[3]] <- as.data.frame(cbind(ST, ganan))
resultados2[[4]] <- as.data.frame(cbind(ST, gananf))
resultados2[[5]] <- result

names(resultados2) <- c('kf', 'ST', 'Ganan_opcion', 'Ganan_forward')
return(resultados2)
}
