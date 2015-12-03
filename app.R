# 
library(shiny)
library(ggplot2)

source("datos.R")
#source("funciones.R")
source("proced.R")
source("gananc.R")

# Aplicaci√ìn Shiny --------------------------------------------------------

ui <- fluidPage(
  fluidRow(
    column(12,
           titlePanel("An√°lisis de coberturas financieras"),
           # informaci√É¬≥n general y descripci√É¬≥n
           mainPanel(
             p("Aplicaci√≥n dise√±ada para analizar distintas estrategias financieras para cobertura 
               de tipo de cambio (USD/MXN) mediante el uso de los derivados: futuros/forwards y opciones."),
             h4("Por: Luis Cortez Virgen, Daniela Guerra Alcal√°, Rodrigo Hern√°ndez Mota y Ra√∫l Romero Barrag√°n"),
             p("Asignatura: Finanzas cuantitativas"),
             h3("Descripci√≥n general"),
             p("A continuaci√≥n se presenta la serie de tiempo diaria de la paridad de la moneda mexicana y el
               d√≥lar estadunidense con 365 datos hist√≥ricos. La metodolog√≠a empleada se basa en calcular 
               trayectorias mediante la determinaci√≥n de la distribuci√≥n real del rendimiento logar√≠tmico  
               de los datos para as√≠ obtener una buena estimaci√≥n del tipo de cambio a 90 d√≠as. Con esta 
               informaci√≥n se eval√∫a el rendimiento de estrategias propuestas con opciones tipo call y 
               futuros/forward."),
            
           # determinaci√≥n arbitraria del precio strike
           numericInput(inputId = "strike", label="Precio strike (k)", 
                        min = 0, max = 100, value = 16.15,step = 0.01),
           sliderInput(inputId = "time",
                       label="D√≠as a simular",
                       value=30, min=1, max=100),
           plotOutput(outputId="gr"),
           textOutput(outputId = "datos"),
           fluidRow(
             column(6,
                    "Opciones",
                    fluidRow(
                      column(6, 
                             "La siguiente gr√°fica muestra las ganancias obtenidas por un 
                             contrato de opci√≥n tipo call en el cual el usuario fija un precio 
                             de ejercicio (strike)."
                             ),
                      column(6,
                             "Gr√°fico"),
                      plotOutput(outputId = "opci")
                    )
             ),
             column(width = 6,
                    "Forward (futuros)",
                    fluidRow(
                      column(6, 
                             "La siguiente gr√°fica muestra las ganancias obtenidas por un 
                             contrato forward en el cual se establece el precio de ejercicio 
                             (strike) como el valor futuro del precio de hoy del activo subyacente."
                            ),
                      column(6,
                             "Gr√°fico"),
                      plotOutput(outputId = "futu")
                    )
                    )
           )
             )
           )
   # column(12,)
    )
)



server <-  function(input, output){
  
  # gr·fico de trayectorias
  output$gr <- renderPlot({
    resultados <- loque(input$time)
    resultados2 <- loque2(resultados[[4]],input$strike, input$time)
    ggplot(environment=environment())+
      geom_line(data = resultados[[1]],aes(ID, Valor, color=Valores),size=0.05)+
      geom_line(data = resultados[[2]], aes(x=ID, y=Promedios),color="dark blue", size=0.7)+
      geom_line(data = resultados[[3]], aes(ID, Precio_original), color="dark orange")+
      geom_hline(aes_string(yintercept=input$strike),color="dark red",size=0.7)+
      geom_hline(aes_string(yintercept=resultados2[[1]]),color="dark blue",size=0.7)+
      theme(legend.position="none")
  })
  
  # gr·fico de opciones
  output$opci <- renderPlot({
    resultados <- loque(input$time)
    resultados2 <- loque2(resultados[[4]],input$strike, input$time)
    ggplot(environment = environment()) + 
      geom_line(data = resultados2$Ganan_opcion, aes(x=ST, y=ganan))+
      geom_hline(aes_string(yintercept = 0), color = 'green')
  })
  
  # gr·fico de futuros 
  output$futu <- renderPlot({
    resultados <- loque(input$time)
    resultados2 <- loque2(resultados[[4]],input$strike, input$time)
    ggplot(environment = environment()) + 
      geom_line(data = resultados2$Ganan_forward, aes(x=ST, y=gananf))+
      geom_hline(aes_string(yintercept = 0), color = 'green')
  })
  
  # datos y ben
  output$datos <- renderText({resultados <- loque(input$time)
                              resultados2 <- loque2(resultados[[4]],input$strike, input$time)
                              paste("El costo de la prima para una opciÛn de tipo de cambio es calculada mediante
                                    una variante de la ecuaciÛn de Black-Scholes. En este caso, la prima tiene un
                                    valor de",resultados2[[5]][3]," MXN por dÛlar. Esta cantidad representa el valor
                                    presente del valor esperado de la ganancia de la opciÛn (diferencia entre precio
                                    strike) bajo la perspectiva que el activo subyacente genera al menos el rendimiento
                                    libre de riesgo. Se procede a simular la realizaciÛn del tipo
                                    de cambio utilizando n trayectorias de valores aleatorios cuya distribuciÛn se
                                    asemeja a la distribuciÛn empÌrica del rendimiento logarÌtmico. 
                                    Con esta simulaciÛn, aplicando especÌficamente la metodologÌa Markov Chain 
                                    Monte Carlo y bajo el algoritmo de Independent Metropolis Hasting se eval˙a el desempeÒo
                                    de una cobertura financiera con derivado opciÛn call y forward. El forward presenta un 
                                    valor esperado de ganancia por unidad de ", resultados2[[5]][6], " y una probabilidad 
                                    de ", resultados2[[5]][7], "de obtener una ganancia al vencimiento. De forma an·loga,
                                    la opciÛn presenta un valor esperado de ganancia por unidad sin contar la prima de ",
                                    resultados2[[5]][1], ", considerando el pago de la prima este valor se ajusta a ",
                                    resultados2[[5]][4], ", con una probabilidad de ejercer de ", resultados2[[5]][2], " y
                                    una probabilidad de obtener ganancia neta de ", resultados2[[5]][4], ".")
                              })
  
  }

shinyApp(ui = ui, server = server)

