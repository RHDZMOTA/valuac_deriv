require(quantmod)
setwd("~/GitHub/valuac_deriv")
# Nombrar acciones de interés - EDITAR{n_acciones}
n_acciones <- c("USD/MXN")
nacciones  <- c("USD/MXN")
n <- length(n_acciones)

# Intervalo de tiempo para los datos - EDITAR{inter_t}
inter_t <- c(toString(as.Date(as.numeric(Sys.Date())-365)),  
             toString(as.Date(Sys.Date())))


getSymbols(n_acciones, src = 'oanda', from = inter_t[1], to = inter_t[2])

# Matriz de precios  - EDITAR{m_precios}
m_precios <- cbind(USDMXN)
nr <- nrow(m_precios)
nc <- ncol(m_precios)

# Determinar si hay NA y filtro automático
cont_na <- 0 #numeric(n)
i <- 1;j <- 1;
while(i <= nr){
  if(is.na(m_precios[i,j])==T){
    cont_na <- cont_na + 1
    i <- i + 1
    j <- 1
  } else {
    if(j == nc){
      j <- 1
      i <- i + 1
    }else{
      j = j + 1
    }
  }
}

if(cont_na > 0){
  m_fprecios <- matrix(0, ncol=nc, nrow = nr - cont_na) # matriz de precios filtrados
  i <- 1; j <- 1; k <- 1;
  while(i <= nr){
    if(is.na(m_precios[i,j])==T){
      i <- i + 1
      j <- 1
    } else {
      if(j == nc){
        m_fprecios[k, ] <- m_precios[i, ]
        i <- i + 1
        j <- 1
        k <- k + 1
      }else{
        j = j + 1
      }
    }
  }
  colnames(m_fprecios) <- n_acciones
}

if(cont_na > 0){
  fechas <- rownames(as.data.frame(m_fprecios))
  precios <- as.matrix(m_fprecios)
} else {
  fechas <- rownames(as.data.frame(m_precios))
  precios <- as.matrix(m_precios)
}
fechas <- as.data.frame(fechas)
# Descargar precios a hoja de cálculo .csv {0,1} - EDITAR{flag}
flag <- 0
if (flag == 1){
  write.csv(precios, 'datos1.csv')
}

# Verificar ausencia de NA
contador <- numeric(ncol(precios))
for(i in 1:nrow(precios)){
  for(j in 1:ncol(precios)){
    if(is.na(precios[i,j])==T){
      contador[j] <- contador[j] + 1
    }
  }
}

# Rendimientos
rendimientos <- matrix(0, nrow = nrow(precios)-1, ncol = (ncol(precios)+1))
rendimientos[, 1] <- c(1:nrow(rendimientos))
rendimientos[, 2:(ncol(precios)+1)] <- log(precios[2:nrow(precios), ] / 
                                             precios[1:(nrow(precios) - 1), ])

medias_rend <- mean(rendimientos[,2])
sigmas_rend  <- sd(rendimientos[,2])

# Precios temporal
precios.temporal <- matrix(0, nrow = nrow(precios), ncol = ncol(precios)+1)
precios.temporal[, 1] <- c(1:nrow(precios))
precios.temporal[, 2:(ncol(precios)+1)] <- precios[,1:ncol(precios)]
precios.temporal <- as.data.frame(precios.temporal)
colnames(precios.temporal) <- c('Tiempo', c(n_acciones))
precios.temporal <- as.data.frame(precios.temporal)

#cat("\014")
cont_na
contador

# Borrar objetos excepto {precios, }
rm(list=setdiff(ls(), c("precios", "n_acciones", "nacciones", 
                        "rendimientos", "precios.temporal","medias_rend",
                        "sigmas_rend", "fechas")))
colnames(precios) <- n_acciones
rownames(precios) <- nrow(c(1:nrow(precios)))
