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



#Importamos la base de datos
df_taxis=read.csv(file="df_taxis.csv")
df_taxis=filter(df_taxis,total_amount<10000)
df_taxis=filter(df_taxis,total_amount>0)
df_taxis=filter(df_taxis,trip_distance>0)
df_taxis=filter(df_taxis,passenger_count>0)
df_taxis=filter(df_taxis,congestion_surcharge>0)



#Creamos las variables nuevas
start_job = mdy_hms(df_taxis$tpep_pickup_datetime)
start_hour = hour(start_job)
start_day = wday(start_job)
finish_job = mdy_hms(df_taxis$tpep_dropoff_datetime)
df_taxis=cbind(df_taxis,start_job,start_day,start_hour,finish_job)

#Eliminamos las columnas que no nos interesan
df_taxis$tpep_pickup_datetime = NULL
df_taxis$tpep_dropoff_datetime = NULL
df_taxis$store_and_fwd_flag = NULL
df_taxis$start_job = NULL
df_taxis$finish_job = NULL


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    dataset <- reactive({
        df_taxis[sample(nrow(df_taxis), input$sampleSize),]
    })
    
    DrawChart <- eventReactive(input$start, {
        chart <- ggplot(dataset()) 
        
        
        # En función del tipo de gráfico elegido se hace un geom_xxx y otro
        if(input$geom == "points") {
            chart <- chart + aes_string(x = input$x, y = input$y) + geom_point(fill=input$fill)
        } else if(input$geom == "boxplot") {
            chart <- chart + aes_string(x = input$xf, y = input$y) + geom_boxplot(fill=input$fill)
        } else if(input$geom == "bar") {
            chart <- chart+ aes_string(x=input$xf) + geom_bar(fill=input$fill)
        } else if(input$geom == "jitter") {
            chart <- chart + aes_string(x = input$x, y = input$y) + geom_jitter(alpha = input$alpha)
        } else if(input$geom == "count") {
            chart <- chart + aes_string(x = input$x, y = input$y) + geom_count(alpha = input$alpha)
        } 
         

        if(input$color != "None")
            chart <- chart + aes_string(color=input$color)

         if(input$facet != "None")
             chart <- chart + facet_wrap(c(input$facet))
         

         if(input$method != "None")
             chart <- chart + geom_smooth(method=input$method)
        
         
        chart <- chart +theme_bw()
        
        
        #LO QUE HEMOS INTENTADO HACER AQUÍ NO FUNCIONA CON EL SHINY
        #Queríamos cambiar los numeros por letras
        #Una posible solución sería crear una columna nueva a la base de datos sustituyendo esta
        
            # if(input$geom=="bar" & input$color=="payment_type")
            #     chart<-chart+scale_x_discrete(labels=c("1"="Credit card","2"="Cash","3"="No charge","4"="Dispute","5"="Unknown","6"="Voided trip"))+
            #     xlab("Tipos de pago")
            # 
            # if(input$geom=="bar" & input$color=="start_day")
            #     chart<-chart+scale_x_discrete(labels=c("1"="Domingo","2"="Lunes","3"="Martes","4"="Miércoles","5"="Jueves","6"="Viernes","7"="Sábado"))+
            #     xlab("Días de la semana")
        
        print(chart)
        
    })


    output$TaxisPlot <- renderPlot({
        DrawChart()
    }, width = 1200, height = 720)
    
    output$TaxisTable <- renderDataTable(dataset())
})

