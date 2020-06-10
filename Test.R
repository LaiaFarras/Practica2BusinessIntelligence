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

if(!require("plyr")) {
  install.packages("plyr")
  library("plyr")
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

if(!require("ggalluvial")) {
  install.packages("ggalluvial")
  library("ggalluvial")
}


### LECTURA DEL ARCHIVO ###
#Data Frame Yellow Taxis 2019 New York
df_taxis=read.csv(file="2019_Yellow_Taxi_Trip_Data.csv", nrows=20000)
str(df_taxis)

#Importem només els que tenen propina i congestió
df_taxis_paid=df_taxis
df_taxis_paid = filter(df_taxis_paid, tip_amount > 0)

df_taxis_congestion=df_taxis
df_taxis_congestion = filter(df_taxis_paid, congestion_surcharge > 0)
str(df_taxis_congestion)


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


# Are there NA values?
sum(is.na(df_taxis))
# No

#Guardem les variables hora, dia... i les adjuntem a la taula
start_job = mdy_hms(df_taxis$tpep_pickup_datetime)
start_hour = hour(start_job)
start_day = wday(start_job)
finish_job = mdy_hms(df_taxis$tpep_dropoff_datetime)
  
df_taxis=cbind(df_taxis,start_job,start_day,start_hour,finish_job)
str(df_taxis)
  
#df_taxis$start_job = as.Date(df_taxis$start_job)
str(df_taxis$start_job)

df_taxis %>% 
  ggplot(aes(as.Date(start_job))) + 
  geom_freqpoly(binwidth = 1)+ # 86400 seconds = 1 day
  theme_bw()+
  xlab('Time y-m')+
  ylab('Number of trips')+
  labs(title="RAW DATA TRIPS",subtitle="Time of each trip in data frame")

#Adjust limits for the following graph
lims <- as.POSIXct(strptime(c("2019-03-01 00:00", "2019-03-03 00:00"), 
                            format = "%Y-%m-%d %H:%M"))

df_taxis %>% 
  ggplot(aes(as.Date(start_job))) + 
  geom_freqpoly(binwidth = 3600)+ # 86400 seconds = 1 day
  theme_bw()+
  scale_x_date(limits=as.Date(lims), breaks = date_breaks("day"),
                   labels=date_format("%y-%m-%d"))+
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



#Relación entre la distancia y el precio a pagar
ggplot(data=df_taxis,aes(x=trip_distance,y=total_amount))+
  geom_point()+
  theme_bw()+
  labs(title="PRECIO - DISTANCIA",subtitle="Relación entre la distancia y el precio")+
  geom_smooth(method='lm') +
  xlim(0,50)

str(df_taxis$PULocationID)

#Study most frequent areas for taxis

PUStationID = count(df_taxis,'PULocationID') #Count number of trips for each zone
PUStationID = PUStationID[PUStationID$freq > nrow(df_taxis)*0.01,]  #Only keep those that represent more than 1%
#PUStationID$PULocationID = as.factor(PUStationID$PULocationID)
#PUStationID = PUStationID %>% arrange(freq)


ggplot(PUStationID, aes(reorder(PULocationID,-freq),freq)) +
  geom_bar(stat='identity') + 
  theme_bw()+
  labs(title="MOST POPULAR ZONES FOR PICK UP",
       subtitle="Zones that concentrate more than 1% of all rides")+
  xlab('Taxi Zones')+
  ylab('Number of trips started')

DOStationID = count(df_taxis,'DOLocationID')
DOStationID = DOStationID[DOStationID$freq > nrow(df_taxis)*0.01,]

ggplot(DOStationID, aes(reorder(DOLocationID,-freq),freq)) +
  geom_bar(stat='identity') + 
  theme_bw()+
  labs(title="MOST POPULAR ZONES FOR DROP OFF",
       subtitle="Zones that concentrate more than 1% of all rides")+
  xlab('Taxi Zones')+
  ylab('Number of trips ended')

#Study the tips that are given in each area
tip_location = df_taxis %>%
  group_by(PULocationID) %>%
  dplyr::summarize(mean_tip = mean(tip_amount, na.rm=TRUE))

high_tip = tip_location[tip_location$mean_tip > mean(tip_location$mean_tip)+sd(tip_location$mean_tip),]

ggplot(data=high_tip,aes(x=reorder(factor(PULocationID),-mean_tip),y=mean_tip))+
  geom_bar(stat='identity')+
  theme_bw()+
  labs(title="BEST ZONES FOR TIPS",subtitle="Showing zones that are above mean + sigma")+
  xlab('Taxi Zones')+
  ylab('Mean tip ($)')

#SOBRECARGA POR CONGESTIÓN CON HORA
#start_hour=as.numeric(df_taxis_congestion$start_hour)
df_taxis %>%
  filter(congestion_surcharge > 0) %>%
  ggplot(df_taxis, mapping=aes(start_hour,fill=factor(start_day))) +
    geom_bar()+
    scale_fill_manual(values=c("#003f5c","#374c80","#7a5195","#bc5090","#ef5675","#ff764a","#ffa600"),
                    breaks =c("1","2","3","4","5","6","7"),
                    labels=c("Domingo","Lunes","Martes","Miércoles","Jueves","Viernes","Sábado"),
                    name="Días de la semana")+
                    labs(title="SOBRECARGA POR CONGESTIÓN",
                    subtitle="Relación entre la sobrecarga por congestión y la hora",
                    x="Hora de pick up",
                    y="Frecuencia de sobrecarga")
    theme_bw()+
    scale_fill_manual(values=c("#003f5c","#374c80","#7a5195","#bc5090","#ef5675","#ff764a","#ffa600"))

#REVISAR! PROPOSTA PROPINES I ZONES I SOBRECÀRREGUES
ggplot(data=df_taxis,aes(x=PULocationID,y=tip_amount))+
  geom_point(color="Purple")+
  theme_bw()+
  labs(title="PRECIO - PICK UP POINT",subtitle="Relación entre el punto de PICK UP y la propina")+
  geom_smooth(method="lm",color="lightpink")

#SOBRECARGA POR CONGESTIÓN CON HORA
start_hour=as.numeric(df_taxis_congestion$start_hour)

# ggplot(df_taxis_congestion, aes(start_hour,fill=factor(start_day))) +
#  geom_bar()+
#   scale_fill_manual(values=c("#003f5c","#374c80","#7a5195","#bc5090","#ef5675","#ff764a","#ffa600"),
#                    # breaks =c("1","2","3","4","5","6","7"),
#                     labels=c("Domingo","Lunes","Martes","Miércoles","Jueves","Viernes","Sábado"),
#                     name="Días de la semana")+
#   labs(title="SOBRECARGA POR CONGESTIÓN",subtitle="Relación entre la sobrecarga por congestión y la hora",
#      x="Hora de pick up",
#      y="Frecuencia de sobrecarga")


df_taxis %>% 
  ggplot(aes(x=start_job, y=congestion_surcharge)) + 
  geom_point()+ 
  scale_y_log10()+
  scale_x_datetime(limits=lims, breaks = date_breaks("hour"),
                   labels=date_format("%d %h:%m"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab('Time y-m-d ')+
  ylab('Congestion surcharge')+
  labs(title="CONGESTION SURCHARGE")

#Numero de pasajeros por viaje
ggplot(df_taxis, aes(fct_infreq(factor(passenger_count))))+
  geom_bar(fill="Brown") + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0))+
  labs(x="Number of passengers",
       y="Number of trips")


#ESTÁ MAL! NO DA INFORMACIÓN! Numero de pasajeros en función de la estación
ggplot(data=df_taxis,aes(x=PULocationID,y=passenger_count))+
  geom_count(color="Pink", alpha=0.5)+
  theme_bw()+
  labs(title="NÚMERO PASAJEROS",
       subtitle = "Numero de pasajeros en función de la PICK UP location",
       x="Pick up location",
       y="Numero de pasajeros")

#Tipos de pago
ggplot(df_taxis, aes(fct_infreq(factor(payment_type))))+
  geom_bar(fill="Pink") + 
  theme_bw()+
  scale_x_discrete(labels=c("1"="Credit card","2"="Cash","3"="No charge","4"="Dispute","5"="Unknown","6"="Voided trip"))+
  theme(axis.text.x = element_text(angle = 0),)+
  labs(title="TIPO DE PAGO",
       subtitle="Distintos tipos de pago",
       x="Tipo de pago",
       y="Frecuencia")


#Numero de personas y tipo de pago
ggplot(data=df_taxis,mapping=aes(y=passenger_count,x=factor(payment_type)))+
  geom_count()+ #color=rainbow(23)
  theme_bw()+
  scale_x_discrete(labels=c("1"="Credit card","2"="Cash","3"="No charge","4"="Dispute","5"="Unknown","6"="Voided trip"))+
  labs(title="TIPO DE PAGO",
       subtitle = "Método de pago en función de los ocupantes del coche",
       y="Cantidad de ocupantes",
       x="Tipo de pago")



#Cantidad pagada y tipo de pago 
ggplot(data=df_taxis,mapping=aes(y=total_amount,x=factor(payment_type)))+
  geom_violin(scale="area")+ #fill='aquamarine'
  theme_bw()+
  scale_x_discrete(labels=c("1"="Credit card","2"="Cash","3"="No charge","4"="Dispute","5"="Unknown","6"="Voided trip"))+
  labs(title="TIPO DE PAGO",
       subtitle = "Método de pago en función de la cantidad a pagar",
       y="Cantidad a pagar",
       x="Tipo de pago")


#Cantidad pagada y propina 
ggplot(data=df_taxis_paid,aes(x=total_amount,y=tip_amount))+
  geom_jitter(alpha=0.4)+  #color=lightpink
  theme_bw()+
  geom_smooth(method='lm')+
  labs(title="CORRELATION",
       subtitle = "Tips against the total amount to pay",
       x="Total amount to pay ($)",
       y="Tip")

cor.test(df_taxis$total_amount, df_taxis$tip_amount, method = "pearson")


ggplot(data=df_taxis_paid,aes(x=trip_distance,y=tip_amount))+
  geom_point()+
  theme_bw()+
  geom_smooth(method='lm')+
  labs(title="CORRELATION",
       subtitle = "Tips against the trip distance",
       x="Trip distance",
       y="Tip ($)")
  
cor.test(df_taxis$trip_distance, df_taxis$tip_amount, method = "pearson")

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


##JA HO HE PUJAT A DALT!!
ggplot(df_taxis_paid, aes(x=start_hour,y=congestion_surcharge,
                     color=factor(start_day))) +
  #geom_point()+
  #geom_jitter()+
  #geom_count(alpha=0.5)+
  scale_fill_brewer(palette="Set3")


#FALTA ACABAR
#y = ifelse(..count.. > nrow(df_taxis)*0.05, ..count.., NA)
df_taxis %>% 
  group_by(PULocationID) %>% 
  mutate(freq = n()) %>% 
  ungroup() %>% 
  filter(freq > nrow(df_taxis)*0.01) %>%
  select(-freq)

ggplot(data = df_taxis,
       aes(axis1 = PULocationID, axis2 = DOLocationID)) +
  scale_x_discrete(limits = c("Pick-up", "Drop off")) +
  xlab("Taxi Zones") +
  geom_alluvium() +
  geom_stratum()  +
  ggtitle("Sankey diagram",
          "Most frequency stations")

