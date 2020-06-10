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
# start_job = mdy_hms(df_taxis$tpep_pickup_datetime)
# start_hour = hour(start_job)
# start_day = wday(start_job)
# finish_job = mdy_hms(df_taxis$tpep_dropoff_datetime)
df_taxis$tpep_pickup_datetime = NULL
df_taxis$tpep_dropoff_datetime = NULL


#df_taxis=cbind(df_taxis,start_job,start_day,start_hour,finish_job)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    dataset <- reactive({
        df_taxis[sample(nrow(df_taxis), input$sampleSize),]
    })
    
    #lims <- as.POSIXct(strptime(c("2019-07-01 00:00", "2019-08-04 00:00"), 
     #                           format = "%Y-%m-%d %H:%M"))
    
    #output$graph <- renderPlot({
     #   ggplot(df_taxis) + 
      #      geom_point(aes(x=start_job, y=input$category)) + 
       #     scale_x_datetime() +
        #    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
         #   xlab('Time y-m-d ') +
           # ylab(input$category) +
            #labs(title="")
    #})

    DrawChart <- eventReactive(input$start, {
        chart <- ggplot(dataset()) 
        
        if(input$geom == "points") {
            chart <- chart + aes_string(x = input$x, y = input$y) + geom_point(alpha = input$alpha)
        } else if(input$geom == "boxplot") {
            chart <- chart + aes_string(x = input$color, y = input$y) + geom_boxplot()
        } 
         
        # if(input$color != "None")
        #     chart <- chart + aes_string(color=input$color)
        
        # if(input$facet != "None")
        #     chart <- chart + facet_wrap(c(input$facet))
        
        if(input$method != "None") 
            chart <- chart + geom_smooth(method=input$method)
        
        print(chart)
        
    })
    
    output$TaxisPlot <- renderPlot({
        DrawChart()
    }, width = 1200, height = 720)
    
    output$TaxisTable <- renderDataTable(dataset())
})

