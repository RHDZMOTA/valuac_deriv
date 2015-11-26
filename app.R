# aplicaciÃ³n shiny para proyecto de finanzas cuantitativas
library(shiny)
library(ggplot2)

source("datos.R")
source("proced.R")


# Aplicación Shiny --------------------------------------------------------


ui <- fluidPage(
  
  titlePanel("Análisis de coberturas financieras"),
  # información general y descripción
  mainPanel(
    p("Aplicación diseñada para analizar distintas estrategias financieras para cobertura 
      de tipo de cambio (MXN/USD) mediante el uso de los derivados: futuros/forwards y opciones."),
    h4("Por: Luis Cortez Virgen, Daniela Guerra Alcalá, Rodrigo Hernández Mota y Raúl Romero Barragán"),
    p("Asignatura: Finanzas cuantitativas"),
    h3("Descripción general"),
    p("A continuación se presenta la serie de tiempo diaria de la paridad de la moneda mexicana y el
      dólar estadunidense con 365 datos históricos. La metodología empleada se basa en calcular 
      trayectorias mediante la determinación de la distribución real del rendimiento logarítmico  
      de los datos para así obtener una buena estimación del tipo de cambio a 90 días. Con esta 
      información se evalúa el rendimiento de estrategias propuestas con opciones tipo call y 
      futuros/forward."),
    p('[agergar párrafo]') ),
  # determinación arbitraria del precio strike
  sliderInput(inputId = "strike",
              label="Precio strike (k)",
              value=16, min=15, max=25),
  plotOutput(outputId="gr")
  
)


server <-  function(input, output){
  #k <- reactive({input$strike^1})
  output$gr <- renderPlot({
    #hist(seq(1,1000,input$strike))
    #k <- get(input$strike);
    ggplot(environment=environment())+
      geom_line(data = russa,aes(ID, Valor, color=Valores),size=0.05)+
      geom_line(data = v_esp, aes(x=ID, y=Promedios),color="dark blue", size=0.7)+
      geom_line(data = russo, aes(ID, Precio_original), color="dark orange")+
      geom_hline(aes_string(yintercept=input$strike),color="dark red",size=0.7)
  })
    
}

shinyApp(ui = ui, server = server)

#GrÃ¡fica con simulaciones
#Volver el K dinÃ¡mico
#Presentar resultados 
#Mostrar anÃ¡lisis para una K arbitrari (probabilidad)
