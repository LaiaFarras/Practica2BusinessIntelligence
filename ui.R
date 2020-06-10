#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny); library(ggplot2)

# df_taxis=read.csv(file="2019_Yellow_Taxi_Trip_Data.csv", nrows=200000)

# # Define UI for application that draws a histogram
# shinyUI(fluidPage(
# 
#     # Application title
#     titlePanel("TLC New York Analysis"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarPanel(
#         selectInput("category", "Select a variable:",
#                     choices=c("congestion_surcharge",
#                               "trip_distance",
#                               "total_amount")),
# 
#         dateRangeInput("daterange3", "Select the date range:",
#                        start  = "2019-08-01",
#                        end    = "2020-01-31",
#                        min    = "2019-01-01",
#                        max    = "2020-01-31",
#                        format = "mm/dd/yy",
#                        separator = " - "),
#     ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#             h1("Dashboard"),
#             p("text",
#             plotOutput("graph")
#         )
#     )
# ))


# numvars <- names(df_taxis[, sapply(df_taxis, is.numeric)])
numvars <- c("passenger_count","trip_distance","PULocationID","DOLocationID","fare_amount","extra","mta_tax","tip_amount","tolls_amount","improvement_surcharge","total_amount","congestion_surcharge","start_hour")

factvars <- c("None","VendorID","RatecodeID","payment_type","passenger_count","start_day")



shinyUI(fluidPage(
    titlePanel("Taxis de NYC"),
    
    # Escull el layout
    sidebarLayout(
        
        # Panell lateral
        sidebarPanel(
            actionButton("start", "Calculate"),
            sliderInput("sampleSize", "Tamaño de la muestra",
                        value = 5000, min = 1000, max = nrow(df_taxis), step = 500, round = 0),
            selectInput('x', 'X', numvars),
            selectInput('y', 'Y', numvars, numvars[2]),
            selectInput("color", "Elige color y variable para boxplot", factvars, "None"),
            selectInput('facet', 'Clasificar en función de variable:', factvars),
            sliderInput("alpha", "Transparency", min = 0, max = 1, value = 0.8),
            radioButtons(inputId = "geom", label = "Tipo de grafico:",
                         choices = c("X Y" = "points",
                                     "Boxplot" = "boxplot",
                                     "Jitter" = "jitter",
                                     "Count"="count"), 
                         selected = "X Y"),
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

