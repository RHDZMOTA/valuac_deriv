# código para obtener datos
library(Quandl)

# intervalo de interés (1 año : 365 días)
inter_t <- c(toString(as.Date(as.numeric(Sys.Date())-365)),  
             toString(as.Date(Sys.Date())))
# el día de ayer
ayer <- toString(as.Date(as.numeric(Sys.Date())-1))

# URL para descargar datos
url <- paste("https://www.quandl.com/api/v3/datasets/CURRFX/USDMXN.csv?start_date=",
             inter_t[1],"&end_date=",inter_t[2])
url2 <- paste("https://www.quandl.com/api/v3/datasets/BDM/SF43878.csv?start_date=",
              ayer,"&end_date=",ayer)
url3 <- paste("https://www.quandl.com/api/v3/datasets/USTREASURY/YIELD.csv?start_date",
              ayer,"&end_date=",ayer)

#descargar csv's 
# download.file(url,"USDMXN.csv")
# download.file(url2,"tiie91.csv")
# download.file(url3,"tbill91.csv")

#leer variables de interés 
tiie91 <- read.csv(file="tiie91.csv",header=TRUE,sep=",",na.strings=TRUE)
tbill91  <- read.csv(file="tbill91.csv",header=TRUE,sep=",",na.strings=TRUE)

r <- tiie91$Value[1]/100
rd <- tiie91$Value[1]/100
rf <- tbill91$X3.Mo[1]/100

#manipulación de datos y determinación de rendimientos logarítmicos
usdmxn <-  read.csv(file="USDMXN.csv",header=TRUE,sep=",",na.strings=TRUE)
usdmxn$Date <- as.Date(usdmxn$Date)
usdmxn[,1:4] <- usdmxn[nrow(usdmxn):1,1:4]
n <- nrow(usdmxn) 
rend <- as.data.frame(usdmxn)
rend$Value[2:n] <- log(usdmxn[2:n,2]/usdmxn[1:(n-1),2])
rend <- rend[2:n,]

