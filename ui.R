#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny); library(ggplot2)

numvars <- c("trip_distance","PULocationID","DOLocationID","fare_amount","extra","mta_tax","tip_amount","tolls_amount","improvement_surcharge","total_amount","congestion_surcharge","start_hour","passenger_count")
factvars <- c("None","VendorID","RatecodeID","payment_type","passenger_count","start_day")



shinyUI(fluidPage(
    titlePanel("Taxis de NYC"),
    # headerPanel("Con esta interfaz podrás analizar los datos de la base de datos sobre los taxis de Nueva York"),

    # Escull el layout
    sidebarLayout(
        
        # Panell lateral
        sidebarPanel(
            actionButton("start", "Calculate"),
            sliderInput("sampleSize", "Tamaño de la muestra",
                        value = 5000, min = 1000, max = nrow(df_taxis), step = 500, round = 0),
            radioButtons(inputId = "geom", label = "Tipo de grafico:",
                         choices = c("X Y (V. X e Y)" = "points",
                                     "Boxplot (V. cat + Y)" = "boxplot",
                                     "Frecuencia (V. cat)"="bar",
                                     "Jitter (V. X e Y)" = "jitter",
                                     "Count (V. X e Y)"="count"),
                                     selected = "X Y"),
             # 
             # #CASO HISTOGRAMA
             # conditionalPanel(
             #     condition="input$geom == 'bar'",
             #     selectInput('x',"Panel x caso histograma",factvars,"None")
             #     ),
             # 
             # #CASO PUNTOS
             # conditionalPanel(
             #     condition='input$geom=="points"',
             #     selectInput('x', 'Panel X caso puntos', numvars)
             # ),

            # #CASO VARIABLES X E Y
            # conditionalPanel(
            #     condition="input$geom!='bar'" & condition="input$geom!='boxplot'",
            #     selectInput('x', 'Variable X', numvars),
            #     selectInput('y', 'variable Y', numvars, numvars[2])
            #     ),


        
                        selectInput('x', 'Variable X', numvars),
                        selectInput('y', 'variable Y', numvars, numvars[2]),
                        selectInput("color", "Variable categórica / color distinto", factvars, "None"),
                        selectInput('facet', 'Clasificar en función de variable ', factvars),
                        sliderInput("alpha", "Transparencia", min = 0, max = 1, value = 0.8),
                        radioButtons(inputId = "method", label = "Método de regresión:",
                          choices = c("None" = "None",
                                      "Simple Linear Regression" = "lm",
                                      "Local Regression" = "loess"),
                          selected = "None")
            ),
            
            # Panell central
            mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Gráficos", plotOutput("TaxisPlot")),
                            tabPanel("Datos", dataTableOutput("TaxisTable"))
            )
        )
    )
))

