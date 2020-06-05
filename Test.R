##=========================================================================##
##                                                                         ##
##  PRACTICA 2 BUSINESS INTELLIGENCE                                       ##
##                                                                         ##
##  @autores: Laia Farràs and Josep Font                                   ##
##                                                                         ##
##=========================================================================##

#### INSTALACIÓN DE PAQUETES ####
if(!require("ggplot2")) {
  install.packages("ggplot2")
  library("ggplot2")
}

if(!require("tidyverse")) {
  install.packages("tidyverse")
  library("tidyverse")
}

if(!require("dplyr")) {
  install.packages("dplyr")
  library("dplyr")
}

if(!require("lubridate")) {
  install.packages("lubridate")
  library("lubridate")
}

if(!require("scales")) {
  install.packages("scales")
  library("scales")
}

### LECTURA DEL ARCHIVO ###
#Data Frame Yellow Taxis 2019 New York
df_taxis=read.csv(file="yellow_tripdata_2019-01.csv", nrows=100000)
str(df_taxis)

#Important varaibles:
#tpep_pickup_datetime
#tpep_dropoff_datetime
#passenger_count
#trip_distance
#RatecodeID
#PULocationID
#DOLocationID
#payment_type
#fare_amount
#extra
#mta_tax
#tip_amount
#tolls_amount
#total_amount

df_taxis$VendorID = NULL
df_taxis$store_and_fwd_flag = NULL
df_taxis$tip_amount = NULL
df_taxis$improvement_surcharge = NULL
df_taxis$congestion_surcharge = NULL

str(df_taxis)

# Are there NA values?
sum(is.na(df_taxis))
# No

start_job = ymd_hms(df_taxis$tpep_pickup_datetime)
finish_job = ymd_hms(df_taxis$tpep_dropoff_datetime)
  
df_taxis=cbind(df_taxis,start_job,finish_job)
str(df_taxis)

ggplot(data=df_taxis, aes(nrows(start_job))) + geom_histogram


start = as.POSIXct("2019/01/01  00:00:00", format="%Y/%m/%d  %H:%M:%S")
end = as.POSIXct("2019/01/2  00:00:00", format="%Y/%m/%d  %H:%M:%S")

start = as.numeric(start)
end = as.numeric(end)

end-start

ggplot(df_taxis, aes(start_job)) + 
  geom_histogram(binwidth=2592000) +
  theme_bw() + xlab(NULL) +
  scale_x_datetime(labels = date_format("%Y/%m"),
                   limits = NULL,
                   breaks=date_breaks(width="3 month"))+
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90))
                   
ggplot(df_taxis, aes(start_job)) + 
  geom_histogram(binwidth=3600) +
  scale_x_datetime(limits = c(as.Date("2019-01-01 00:00:00"),as.Date("2019-01-01 24:00:00")))

 # theme_bw() + xlab(NULL) +
 # scale_x_datetime(labels = date_format("%Y/%m/%d %h/%m/%s"),
  #                 limits = c(as.Date("2019-01-01 00:00:00"),
  #                            as.Date("2019-01-01 24:00:00"),
  #                 breaks=date_breaks(width="1 hour")))


#Outliers analysis





#PLOT


ggplot(data=df_taxis,aes(x='trip_distance',y='total_amount'))+
  geom_point(aes(alpha=0.1))+
  labs(title="PRECIO - DISTANCIA",subtitle="Relación entre la distancia y el precio")


