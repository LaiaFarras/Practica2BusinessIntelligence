##=========================================================================##
##                                                                         ##
##  PRACTICA 2 BUSINESS INTELLIGENCE                                       ##
##                                                                         ##
##  @autores: Laia Farràs and Josep Font                                   ##
##  IQS Barcelona School of Engineering                                    ##
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

if(!require("ggraph")) {
  install.packages("ggraph")
  library("ggraph")
}

if(!require("GGally")) {
  install.packages("GGally")
  library("GGally")
}


### LECTURA DEL ARCHIVO ###
#Data Frame Yellow Taxis 2019 New York
df_taxis=read.csv(file="2019_Yellow_Taxi_Trip_Data.csv", nrows=200000)
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
#df_taxis$VendorID = NULL

str(df_taxis)

# Are there NA values?
sum(is.na(df_taxis))
# No

start_job = mdy_hms(df_taxis$tpep_pickup_datetime)
start_hour = hour(start_job)
start_day = wday(start_job)
finish_job = mdy_hms(df_taxis$tpep_dropoff_datetime)
  
df_taxis=cbind(df_taxis,start_job,start_day,start_hour,finish_job)
str(df_taxis)
  
df_taxis$start_job = as.Date(df_taxis$start_job)

df_taxis %>% 
  ggplot(aes(as.Date(start_job))) + 
  geom_freqpoly(binwidth = 1)+ # 86400 seconds = 1 day
  xlab('Time y-m')+
  ylab('Number of trips')+
  labs(title="RAW DATA TRIPS",subtitle="Time of each trip in data frame")

#Adjust limits for the following graph
lims <- as.POSIXct(strptime(c("2019-07-01 00:00", "2019-08-04 00:00"), 
                            format = "%Y-%m-%d %H:%M"))

df_taxis %>% 
  ggplot(aes(start_job)) + 
  geom_freqpoly(binwidth = 3600)+ # 86400 seconds = 1 day
  scale_y_log10()+
  scale_x_date(limits=lims, breaks = date_breaks("day"),
                   labels=date_format("%y-%m-%d"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab('Time y-m-d ')+
  ylab('Number of trips')+
  labs(title="RAW DATA TRIPS",subtitle="Time of each trip in data frame")

#Outliers analysis

#PLOT PRECIO_DISTANCIA

#Hacer analisis para estudiar valores atípicos
#Viajes con distancia 0
#Viajes con valor 0
#Valores negativos

sum(is.null(df_taxis$trip_distance))
sum(is.null(df_taxis$total_amount))

df_taxis = filter(df_taxis, total_amount > 0)

ggplot(data=df_taxis,aes(x=trip_distance,y=total_amount))+
  geom_point()+
  labs(title="PRECIO - DISTANCIA",subtitle="Relación entre la distancia y el precio")+
  geom_smooth(method='lm') +
  xlim(0,50)
  #scale_x_log10()
  #ylim(0,500)

str(df_taxis$PULocationID)

#PLOT TRAYECTOS FREQUENTES
taxi_zones_PU=sort(unique(df_taxis$PULocationID))
taxi_zones_DO=sort(unique(df_taxis$DOLocationID))

ggplot(df_taxis, aes(fct_infreq(factor(PULocationID)))) +
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 90))+
  labs(title="ZONAS TLC MÁS FRECUENTES PARA INICIO",
       subtitle="Número de viajes pedidos en cada zona")

ggplot(df_taxis, aes(fct_infreq(factor(DOLocationID)))) +
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 90))+
  labs(title="ZONAS TLC MÁS FRECUENTES PARA FINALIZACIÓN",
       subtitle="Número de viajes terminados en cada zona")

## INFO ##
#En comptes de carregarnos propina i surcharge per embussos

#PROPOSTA PROPINES I ZONES I SOBRECÀRREGUES

ggplot(data=df_taxis,aes(x=PULocationID,y=tip_amount))+
  geom_point(color="Purple")+
  labs(title="PRECIO - PICK UP POINT",subtitle="Relación entre el punto de PICK UP y la propina")+
  geom_smooth(method="lm",color="lightpink")

ggplot(data=df_taxis,aes(x=trip_distance,y=congestion_surcharge))+
  geom_point(color="Purple")+
  labs(title="SOBRECARGA POR CONGESTIÓN",subtitle="Relación entre la sobrecarga por congestión y la distancia de trayecto")+
  geom_smooth(method="lm",color="lightpink")

lims <- as.POSIXct(strptime(c("2019-07-17 00:00", "2019-07-18 00:00"), 
                            format = "%Y-%m-%d %H:%M"))

df_taxis %>% 
  ggplot(aes(x=start_job, y=congestion_surcharge)) + 
  geom_point()+ 
  scale_y_log10()+
  scale_x_datetime(limits=lims, breaks = date_breaks("hour"),
                   labels=date_format("%d %h:%m"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab('Time y-m-d ')+
  ylab('Congestion surcharge')+
  labs(title="CONGESTION SURCHARGE")

str(df_taxis)
# MODELO: PREDECIR PRECIO DEL VIAJE

sum(is.na(df_taxis$total_amount))
sum(is.na(df_taxis$passenger_count))
sum(is.na(df_taxis$trip_distance))
sum(is.na(df_taxis$start_hour))
sum(is.na(df_taxis$start_day))

df_taxis$passenger_count %>% drop_na(df_taxis$passenger_count)
str(df_taxis$passenger_count)

df %>% filter(!is.na(df_taxis$passenger_count))
sum(is.na(df_taxis$passenger_count))

modelo <- (lm(formula = total_amount ~ passenger_count + trip_distance +
                start_hour + start_day , data = df_taxis))
summary(modelo)

ggplot(df_taxis, aes(x=start_hour,y=congestion_surcharge,
                     color=factor(start_day))) +
  geom_point()+
  geom_jitter()+
  scale_fill_brewer(palette="Set3")

