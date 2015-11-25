# aplicación shiny para proyecto de finanzas cuantitativas

library(shiny)
library(ggplot2)
library(Quandl)
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
download.file(url,"USDMXN.csv")
download.file(url2,"tiie91.csv")
tiie91 <- read.csv(file="tiie91.csv",header=TRUE,sep=",",na.strings=TRUE)
usdmxn <-  read.csv(file="USDMXN.csv",header=TRUE,sep=",",na.strings=TRUE)
usdmxn$Date <- as.Date(usdmxn$Date)
usdmxn[,1:4] <- usdmxn[nrow(usdmxn):1,1:4]

#ggplot(data=usdmxn, aes(x=Date, y=Rate))+geom_line()

# rendimientos logarítmicos
n <- nrow(usdmxn) 
rend <- as.data.frame(usdmxn)
rend$Value[2:n] <- log(usdmxn[2:n,2]/usdmxn[1:(n-1),2])
rend <- rend[2:n,]

#ggplot(data=rend, aes(x=Date, y=Value))+geom_line()


# Aplicación Shiny --------------------------------------------------------


ui <- fluidPage(
  
  titlePanel("Análisis de coberturas financieras"),
  # información general y descripción
  mainPanel(
    p("Aplicación diseñada como apoyo en el cálculo de derivados 
      financieros y sus distintos precios de ejercicio recomendados."),
    h4("Por: Luis Cortez Virgen, Daniela Guerra Alcalá, Rodrigo Hernández Mota y Raúl Romero Barragán"),
    p("Finanzas Cuantitativas"),
    p(),
    p('perri')
    ),
  plotOutput(outputId="gr")
  
)


server <-  function(input, output){
  output$gr <- renderPlot({
    ggplot(data=usdmxn, aes(x=Date, y=Rate))+geom_line()
  }
  )
}

shinyApp(ui = ui, server = server)

#Gráfica con simulaciones
#Volver el K dinámico
#Presentar resultados 
#Mostrar análisis para una K arbitrari (probabilidad)
