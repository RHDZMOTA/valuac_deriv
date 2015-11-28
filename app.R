# aplicaciÃÂ³n shiny para proyecto de finanzas cuantitativas
library(shiny)
library(ggplot2)

source("datos.R")
#source("funciones.R")
source("proced.R")
source("gananc.R")

# AplicaciÃ³n Shiny --------------------------------------------------------

ui <- fluidPage(
  fluidRow(
    column(12,
           titlePanel("Análisis de coberturas financieras"),
           # información general y descripción
           mainPanel(
             p("AplicaciÃ³n diseÃ±ada para analizar distintas estrategias financieras para cobertura 
               de tipo de cambio (MXN/USD) mediante el uso de los derivados: futuros/forwards y opciones."),
             h4("Por: Luis Cortez Virgen, Daniela Guerra AlcalÃ¡, Rodrigo HernÃ¡ndez Mota y RaÃºl Romero BarragÃ¡n"),
             p("Asignatura: Finanzas cuantitativas"),
             h3("DescripciÃ³n general"),
             p("A continuaciÃ³n se presenta la serie de tiempo diaria de la paridad de la moneda mexicana y el
               dÃ³lar estadunidense con 365 datos histÃ³ricos. La metodologÃ?a empleada se basa en calcular 
               trayectorias mediante la determinaciÃ³n de la distribuciÃ³n real del rendimiento logarÃ?tmico  
               de los datos para asÃ? obtener una buena estimaciÃ³n del tipo de cambio a 90 dÃ?as. Con esta 
               informaciÃ³n se evalÃºa el rendimiento de estrategias propuestas con opciones tipo call y 
               futuros/forward."),
             p('[agergar pÃ¡rrafo]') ),
           # determinaciÃ³n arbitraria del precio strike
           numericInput(inputId = "strike", label="Precio strike (k)", 
                        min = 0, max = 100, value = 16,step = 0.01),
           sliderInput(inputId = "time",
                       label="D�?as para simular",
                       value=30, min=1, max=100),
           plotOutput(outputId="gr"),
           fluidRow(
             column(6,
                    "Opciones",
                    fluidRow(
                      column(6, 
                             "Teor�?a",
                             textOutput(outputId =  "yolo")),
                      column(6,
                             "Gráfico")
                    )
             ),
             column(width = 6,
                    "Forward (futuros)"
             )
           )
             )
           )
    )



server <-  function(input, output){
  output$gr <- renderPlot({
    resultados <- loque(input$time)
    kf <- loque2(resultados[[4]],input$strike, input$time)
    ggplot(environment=environment())+
      geom_line(data = resultados[[1]],aes(ID, Valor, color=Valores),size=0.05)+
      geom_line(data = resultados[[2]], aes(x=ID, y=Promedios),color="dark blue", size=0.7)+
      geom_line(data = resultados[[3]], aes(ID, Precio_original), color="dark orange")+
      geom_hline(aes_string(yintercept=input$strike),color="dark red",size=0.7)+
      geom_hline(aes_string(yintercept=kf),color="dark blue",size=0.7)
  })
  output$yolo <- renderText(paste("Tipo", class(input$strike)))
}

shinyApp(ui = ui, server = server)

