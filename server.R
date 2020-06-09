#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
if(!require("dplyr")) {
    install.packages("dplyr")
    library("dplyr")
}

if(!require("plotly")) {
    install.packages("plotly")
    library("plotly")
}

if(!require("ggplot2")) {
    install.packages("ggplot2")
    library("ggplot2")
}

if(!require("lubridate")) {
    install.packages("lubridate")
    library("lubridate")
}

df_taxis=read.csv(file="2019_Yellow_Taxi_Trip_Data.csv", nrows=20000)
start_job = mdy_hms(df_taxis$tpep_pickup_datetime)
df_taxis=cbind(df_taxis,start_job)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    lims <- as.POSIXct(strptime(c("2019-07-01 00:00", "2019-08-04 00:00"), 
                                format = "%Y-%m-%d %H:%M"))
    
    output$graph <- renderPlot({
        ggplot(df_taxis) + 
            geom_point(aes(x=start_job, y=input$category)) + 
            scale_x_datetime() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            xlab('Time y-m-d ') +
            ylab(input$category) +
            labs(title="")
    })
})
