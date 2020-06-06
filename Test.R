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

if(!require("ggraph")) {
  install.packages("ggraph")
  library("ggraph")
}

if(!require("ggridges")) {
  install.packages("ggridges")
  library("ggridges")
}

if(!require("GGally")) {
  install.packages("GGally")
  library("GGally")
}


### LECTURA DEL ARCHIVO ###
#Data Frame Yellow Taxis 2019 New York
df_taxis=read.csv(file="2019_Yellow_Taxi_Trip_Data.csv", nrows=1000)
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

ggplot(df_taxis, aes(start_job)) + 
  geom_histogram(binwidth=604800) +
  theme_bw() + xlab(NULL) +
  scale_x_datetime(labels = date_format("%M/%Y"),
                   limits = NULL,
                   breaks=date_breaks(width="1 week"))+
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90))

lims = as.POSIXct(strptime(c("2019-01-01 00:00:00","2019-02-01 00:00:00"),
                format = "%Y-%M-%d %h:%m:%s"))


#NO FUNCIONA
ggplot(df_taxis, aes(start_job)) + 
  geom_histogram(binwidth=86400) +
  scale_x_datetime(limits = lims)

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
  geom_smooth(method="gam",color="lightpink")

ggplot(data=df_taxis,aes(x=trip_distance,y=congestion_surcharge))+
  geom_point(color="Purple")+
  labs(title="SOBRECARGA POR CONGESTIÓN",subtitle="Relación entre la sobrecarga por congestión y la distancia de trayecto")+
  geom_smooth(method="gam",color="lightpink")

str(df_taxis)
# MODELO: PREDECIR PRECIO DEL VIAJE

modelo <- lm(total_amount ~ passenger_count + trip_distance +  + asesinatos +
               universitarios + heladas + area + densidad_pobl, data = df_taxis)
summary(modelo)

ggplot(df_taxis, aes(x=start_hour,y=congestion_surcharge,
                     color=factor(start_day))) +
  geom_point()+
  geom_jitter()+
  scale_fill_brewer(palette="Set3")

ggplot(df_taxis, aes(x = start_hour, y = congestion_surcharge)) + geom_density_ridges()

sum(is.na(df_taxis$start_day))

