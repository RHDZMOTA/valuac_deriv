#Archivo de funciones ordenado :)


# Función Inversa ---------------------------------------------------------
# agregar descrip
# u: número con distribución uniforme
# interv: límite inferior de las clases del histograma de frecuencia
#freq_acum: vector de frecuencia acumulada
inversa <- function(u, interv, freq_acum){
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


# Función Kernel Gaussiano--------------------------------------------------
# agregar descrp. kernel individual de cada dato 
# equis: dato de los registros empíricos
# n: tamaño de  muestra
# h: intervalo (definido)
# x: secuencia que representa el eje x
kernel <- function(equis, n, h, x){
  kero <- exp(-(x-equis)^2/(2*h^2))/(sqrt(2*pi)*(n-1)*h)
  return(kero)
}


# Función Densidad Objetivo Estimada --------------------------------------
# agregar descrip.
# u2: número aleatorio con distribución unfirme
# x: secuencia que representa el eje x
#fest: vector que contiene la frecuencia generada por el kernel
f<-function(u2,x,fest){
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


# Función Densidad Propuesta ----------------------------------------------
#agregar descrip
#u3: número aleatorio con distribución uniforme
#interv: límite inferior de las clases del histograma de frecuencia
#freq_rel: vector que contiene la frecuencia relativa de la variable 
qu<-function(u3,interv,freq_rel){
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


# Función Metropolis Hasting ----------------------------------------------
#agregar descrip.
#Selección de nums aleatorios (y). Se eligen los que cumplen alpha, se adecúan más.
#Los que "pasen la prueba" serán la estimación para el siguiente día.
#Función RDRL. Generará aleatorios y discriminará.
#e: dato anterior (de referencia para simular)
#deseos1: número de trayectorias simuladas (Deseadas) 
RDRL <- function(e,deseos1, x, fest, interv, freq_rel, freq_acum){#d=un vector de aleatorios, e = ultimo dato
  y_esti<-numeric()
  equis <- runif(deseos1)
  for (i in 1:deseos1){
    y[i]<-inversa(runif(1),interv, freq_acum)
    d<-y[i]
    #e<-rend$Value[n-1]
    alpha<-(f(d,x,fest)*qu(e, interv,freq_rel))/(f(e,x,fest)*qu(d, interv,freq_rel))
    y_esti[i]<-e+(d-e)*(equis[i]<alpha)
  }
  return(y_esti)
}
