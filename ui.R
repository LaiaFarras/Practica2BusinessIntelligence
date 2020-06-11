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
    colores=0,

    # Escull el layout
    sidebarLayout(
        
        # Panell lateral
        sidebarPanel(
            actionButton("start", "Calculate"),
            sliderInput("sampleSize", "Tamaño de la muestra",
                        value = 5000, min = 1000, max = nrow(df_taxis), step = 500, round = 0),
            radioButtons(inputId = "geom", label = "Tipo de grafico:",
                         choices = c("X Y" = "points",
                                     "Boxplot" = "boxplot",
                                     "Frecuencia"="bar",
                                     "Jitter" = "jitter",
                                     "Count"="count"),
                                     selected = "X Y"),

             #CASO BARRAS
             conditionalPanel(
                 condition="input.geom == 'bar'",
                 selectInput('x',"Variable x diagrama barras",factvars,"None"),
                 selectInput('facet', 'Clasificar en función de variable ', factvars),
                 radioButtons(inputId="fill",label="Color de relleno",
                              choices=c("Rosa"="lightpink",
                                        "Azul"="blue",
                                        "Verde"="green",
                                        "Rojo"="red"))),
            

            #CASO BOXPLOT
            conditionalPanel(
                condition="input.geom == 'boxplot'",
                selectInput('x',"Variable x",factvars,"None"),
                selectInput('y', 'variable Y', numvars, numvars[2]),
                selectInput('facet', 'Clasificar en función de variable ', factvars)),
  
            
            
             #CASO PUNTOS
             conditionalPanel(
                 condition='input.geom=="points"',
                 selectInput('x', 'Variable X', numvars),
                 selectInput('y', 'variable Y', numvars, numvars[2]),
                 selectInput("color", "Color de la variable", factvars, "None"),
                 selectInput('facet', 'Clasificar en función de variable ', factvars),
                 sliderInput("alpha", "Transparencia", min = 0, max = 1, value = 0.8),
                 radioButtons(inputId = "method", label = "Método de regresión:",
                              choices = c("None" = "None",
                                          "Simple Linear Regression" = "lm",
                                          "Local Regression" = "loess"),
                              selected = "None")),
            
            

            #CASO JITTER
            conditionalPanel(
                condition='input.geom=="jitter"',
                selectInput('x', 'Variable X', numvars),
                selectInput('y', 'variable Y', numvars, numvars[2]),
                selectInput("color", "Color de la variable", factvars, "None"),
                selectInput('facet', 'Clasificar en función de variable ', factvars),
                sliderInput("alpha", "Transparencia", min = 0, max = 1, value = 0.8),
                radioButtons(inputId = "method", label = "Método de regresión:",
                             choices = c("None" = "None",
                                         "Simple Linear Regression" = "lm",
                                         "Local Regression" = "loess"),
                             selected = "None")),
            
            #CASO COUNT
            conditionalPanel(
                condition='input.geom=="count"',
                selectInput('x', 'Variable X', numvars),
                selectInput('y', 'variable Y', numvars, numvars[2]),
                selectInput("color", "Color de la variable", factvars, "None"),
                selectInput('facet', 'Clasificar en función de variable ', factvars),
                sliderInput("alpha", "Transparencia", min = 0, max = 1, value = 0.8),
                radioButtons(inputId = "method", label = "Método de regresión:",
                             choices = c("None" = "None",
                                         "Simple Linear Regression" = "lm",
                                         "Local Regression" = "loess"),
                             selected = "None")),

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

