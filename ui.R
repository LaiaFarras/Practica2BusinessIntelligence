#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)

df_taxis=read.csv(file="2019_Yellow_Taxi_Trip_Data.csv", nrows=200000)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("TLC New York Analysis"),

    # Sidebar with a slider input for number of bins
    sidebarPanel(
        selectInput("category", "Select a variable:",
                    choices=c("congestion_surcharge",
                              "trip_distance",
                              "total_amount")),
        
        dateRangeInput("daterange3", "Select the date range:",
                       start  = "2019-08-01",
                       end    = "2020-01-31",
                       min    = "2019-01-01",
                       max    = "2020-01-31",
                       format = "mm/dd/yy",
                       separator = " - "),
    ),

        # Show a plot of the generated distribution
        mainPanel(
            h1("Dashboard"),
            p("text",
            plotOutput("graph")
        )
    )
))
