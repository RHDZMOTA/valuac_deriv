# 
library(shiny)
library(ggplot2)

source("datos.R")
#source("funciones.R")
source("proced.R")
source("gananc.R")

# AplicaciÓn Shiny --------------------------------------------------------

ui <- fluidPage(
  fluidRow(
    column(12,
           titlePanel("Análisis de coberturas financieras"),
           # informaciÃ³n general y descripciÃ³n
           mainPanel(
             p("Aplicación diseñada para analizar distintas estrategias financieras para cobertura 
               de tipo de cambio (USD/MXN) mediante el uso de los derivados: futuros/forwards y opciones."),
             h4("Por: Luis Cortez Virgen, Daniela Guerra Alcalá, Rodrigo Hernández Mota y Raúl Romero Barragán"),
             p("Asignatura: Finanzas cuantitativas"),
             h3("Descripción general"),
             p("A continuación se presenta la serie de tiempo diaria de la paridad de la moneda mexicana y el
               dólar estadunidense con 365 datos históricos. La metodología empleada se basa en calcular 
               trayectorias mediante la determinación de la distribución real del rendimiento logarítmico  
               de los datos para así obtener una buena estimación del tipo de cambio a 90 días. Con esta 
               información se evalúa el rendimiento de estrategias propuestas con opciones tipo call y 
               futuros/forward."),
            
           # determinación arbitraria del precio strike
           numericInput(inputId = "strike", label="Precio strike (k)", 
                        min = 0, max = 100, value = 16.65,step = 0.01),
           sliderInput(inputId = "time",
                       label="Días a simular",
                       value=30, min=1, max=100),
           plotOutput(outputId="gr"),
           fluidRow(
             column(6,
                    "Opciones",
                    fluidRow(
                      column(6, 
                             "La siguiente gráfica muestra las ganancias obtenidas por un contrato de opción tipo call en el cual el usuario fija un precio de ejercicio (strike)."
                             ),
                      column(6,
                             "Gráfico"),
                      plotOutput(outputId = "opci")
                    )
             ),
             column(width = 6,
                    "Forward (futuros)",
                    fluidRow(
                      column(6, 
                             "La siguiente gráfica muestra las ganancias obtenidas por un contrato forward en el cual se establece el precio de ejercicio (strike) como el valor futuro del precio de hoy del activo subyacente."
                            ),
                      column(6,
                             "Gráfico"),
                      plotOutput(outputId = "futu")
                    )
                    )
           )
             )
           )
    )
)



server <-  function(input, output){
  output$gr <- renderPlot({
    resultados <- loque(input$time)
    resultados2 <- loque2(resultados[[4]],input$strike, input$time)
    ggplot(environment=environment())+
      geom_line(data = resultados[[1]],aes(ID, Valor, color=Valores),size=0.05)+
      geom_line(data = resultados[[2]], aes(x=ID, y=Promedios),color="dark blue", size=0.7)+
      geom_line(data = resultados[[3]], aes(ID, Precio_original), color="dark orange")+
      geom_hline(aes_string(yintercept=input$strike),color="dark red",size=0.7)+
      geom_hline(aes_string(yintercept=resultados2[[1]]),color="dark blue",size=0.7)
  })
  output$yolo <- renderText(paste("Tipo", class(input$strike)))
  output$opci <- renderPlot({
    resultados <- loque(input$time)
    resultados2 <- loque2(resultados[[4]],input$strike, input$time)
    ggplot(environment = environment()) + 
      geom_line(data = resultados2$Ganan_opcion, aes(x=ST, y=ganan))+
      geom_hline(aes_string(yintercept = 0), color = 'green')
  })
  output$futu <- renderPlot({
    resultados <- loque(input$time)
    resultados2 <- loque2(resultados[[4]],input$strike, input$time)
    ggplot(environment = environment()) + 
      geom_line(data = resultados2$Ganan_forward, aes(x=ST, y=gananf))+
      geom_hline(aes_string(yintercept = 0), color = 'green')
  })
  
  
  }

shinyApp(ui = ui, server = server)

