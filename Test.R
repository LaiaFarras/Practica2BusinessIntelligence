##=========================================================================##
##                                                                         ##
##  PRACTICA 2 BUSINESS INTELLIGENCE                                       ##
##                                                                         ##
##  @autores: Laia Farràs and Josep Font                                   ##
##  IQS Barcelona School of Engineering                                    ##
##                                                                         ##
##=========================================================================##


#### PACKAGES INSTALLATION ####
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


### LECTURA DEL ARCHIVO ###
#Data Frame Yellow Taxis 2019 New York
df_taxis=read.csv(file="df_taxis.csv")
#Eliminate trips without distance
df_taxis= filter(df_taxis, trip_distance > 0)
#Total amount must be positive

#Total amount can't be zero
df_taxis= filter(df_taxis, total_amount > 0)
str(df_taxis)

#Number of passengers can't be zero
df_taxis= filter(df_taxis, passenger_count > 0)
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
minutes = as.numeric(difftime(finish_job,start_job,units='min'))

df_taxis=cbind(df_taxis,start_job,start_day,start_hour,finish_job,minutes)

#Trips must have a time of at least 1 minute
df_taxis= filter(df_taxis, minutes > 1)


str(df_taxis)
  
#df_taxis$start_job = as.Date(df_taxis$start_job)
str(df_taxis$start_job)

#PLOT 1
df_taxis %>% 
  ggplot(aes(as.Date(start_job))) + 
  geom_freqpoly(binwidth = 1)+ # 86400 seconds = 1 day
  theme_bw()+
  xlab('Time y-m')+
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
#PLOT 2
ggplot(data=df_taxis,aes(x=trip_distance,y=total_amount))+
  geom_point()+
  theme_bw()+
  labs(title="TOTAL AMOUNT - DISTANCE",subtitle="Comparison through scatter plot")+
  geom_smooth(method='lm') +
  xlim(0,50)+
  xlab('Distance (miles)')+
  ylab('Total amount ($)')

str(df_taxis$PULocationID)

#Study most frequent areas for taxis

PUStationID = count(df_taxis,'PULocationID') #Count number of trips for each zone
PUStationID = PUStationID[PUStationID$freq > nrow(df_taxis)*0.01,]  #Only keep those that represent more than 1%
#PUStationID$PULocationID = as.factor(PUStationID$PULocationID)
#PUStationID = PUStationID %>% arrange(freq)

#PLOT 3
ggplot(PUStationID, aes(reorder(PULocationID,-freq),freq)) +
  geom_bar(stat='identity') + 
  theme_bw()+
  labs(title="MOST POPULAR ZONES FOR PICK UP",
       subtitle="Zones that concentrate more than 1% of all rides")+
  xlab('Taxi Zones')+
  ylab('Number of trips started')

DOStationID = count(df_taxis,'DOLocationID')
DOStationID = DOStationID[DOStationID$freq > nrow(df_taxis)*0.01,]

#PLOT 4
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

#PLOT 5
ggplot(data=high_tip,aes(x=reorder(factor(PULocationID),-mean_tip),y=mean_tip))+
  geom_bar(stat='identity')+
  theme_bw()+
  labs(title="BEST ZONES FOR TIPS",subtitle="Showing zones that are above mean + sigma")+
  xlab('Taxi Zones')+
  ylab('Mean tip ($)')

#Scatter plot number of trips and mean tips
PUStationID = count(df_taxis,'PULocationID') #Count number of trips for each zone
df_mean_tip=cbind(PUStationID, tip_location[,2])

#PLOT 6
ggplot(data=df_mean_tip, aes(x=freq,y=mean_tip, label=PULocationID))+
  geom_point()+
  geom_text(aes(label=ifelse(mean_tip>mean(mean_tip)*2 & freq > mean(freq),
                             as.character(PULocationID),'')),hjust=-0.3, vjust=-0.3)+
  labs(title="BEST ZONES TO WORK FOR TIPS",
       subtitle="Comparing the demand for trips and the average tips")+
  xlab('Number of trips')+
  ylab('Mean tip ($)')

#Scatter plot number of trips and mean amount paid
mean_amount = df_taxis %>%
  group_by(PULocationID) %>%
  dplyr::summarize(mean_amount = mean(total_amount, na.rm=TRUE))

df_mean_amount=cbind(PUStationID, mean_amount[,2])
#PLOT 7
ggplot(data=df_mean_amount, aes(x=freq,y=mean_amount, label=PULocationID))+
  geom_point()+
  geom_text(aes(label=ifelse(mean_amount>mean(mean_amount) & freq > mean(freq),
                             as.character(PULocationID),'')),hjust=-0.3, vjust=-0.3)+
  labs(title="BEST ZONES TO WORK FOR TIPS",
       subtitle="Comparing the demand for trips and the average amount paid")+
  xlab('Number of trips')+
  ylab('Mean amount paid ($)')

#Study most profitable zones per minute
PUStationID = count(df_taxis,'PULocationID') #Count number of trips for each zone
price_mile = df_taxis %>%
  group_by(PULocationID) %>%
  dplyr::summarize(price_mile = mean(total_amount/as.numeric(difftime(finish_job,start_job,units='min'))
                                     , na.rm=TRUE))

df_price_mile=cbind(PUStationID, price_mile[,2])

ggplot(data=df_price_mile, aes(x=freq,y=price_mile, label=PULocationID))+
  geom_point()+
  geom_text(aes(label=ifelse(price_mile>mean(price_mile) & freq>mean(freq),
                             as.character(PULocationID),'')),hjust=-0.3, vjust=-0.3)+
  labs(title="BEST ZONES TO WORK FOR TOTAL AMOUNT",
       subtitle="Comparing the demand for trips and the average amount paid")+
  coord_cartesian(ylim=c(0.75,2.5))+
  xlab('Number of trips')+
  ylab('Mean amount paid ($)/minute')

#SOBRECARGA POR CONGESTIÓN CON HORA
#start_hour=as.numeric(df_taxis_congestion$start_hour)
#PLOT 9
df_taxis %>%
  filter(congestion_surcharge > 0) %>%
  ggplot(df_taxis, mapping=aes(start_hour,fill=factor(start_day))) +
    geom_bar()+
    theme_bw()+
    scale_fill_manual(values=c("#003f5c","#374c80","#7a5195","#bc5090","#ef5675","#ff764a","#ffa600"),
                    breaks =c("1","2","3","4","5","6","7"),
                    labels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),
                    name="Day of the week")+
                    labs(title="CONGESTION",
                    subtitle="Daytime graph comparing congestion in different days of the week",
                    x="Pick up hour",
                    y="Congestion frequency")
    
    scale_fill_manual(values=c("#003f5c","#374c80","#7a5195","#bc5090","#ef5675","#ff764a","#ffa600"))


#SOBRECARGA POR CONGESTIÓN CON HORA
start_hour=as.numeric(df_taxis_congestion$start_hour)


#PLOT 10
#Numero de pasajeros por viaje
ggplot(df_taxis, aes(fct_infreq(factor(passenger_count))))+
  geom_bar() + 
  theme_bw()+
  scale_y_continuous(breaks=seq(0,140000,10000),labels=comma)+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="Classification of trips by number of passengers",
       x="Number of passengers",
       y="Number of trips")

#PLOT 11
#ESTÁ MAL! NO DA INFORMACIÓN! Numero de pasajeros en función de la estación
ggplot(data=df_taxis,aes(x=PULocationID,y=passenger_count))+
  geom_count(alpha=0.5)+ #color='Pink'
  theme_bw()+
  labs(title="NUMBER OF PASSENGERS",
       subtitle = "Comparing passenger count on each taxi zone",
       x="Pick up location",
       y="Number of passengers per trip")

#Tipos de pago
#PLOT 12
ggplot(df_taxis, aes(fct_infreq(factor(payment_type))))+
  geom_bar() + #fill='Pink'
  theme_bw()+
  scale_y_continuous(breaks=seq(0,150000,25000),labels=comma)+
  scale_x_discrete(labels=c("1"="Credit card","2"="Cash","3"="No charge","4"="Dispute","5"="Unknown","6"="Voided trip"))+
  theme(axis.text.x = element_text(angle = 0),)+
  labs(title="PAYMENT METHODS",
       subtitle="Comparing the use of different payment methods",
       x="Payment type",
       y="Frecuency")


#Numero de personas y tipo de pago
#PLOT 13
ggplot(data=df_taxis,mapping=aes(y=passenger_count,x=factor(payment_type)))+
  geom_count()+ #color=rainbow(23)
  theme_bw()+
  scale_y_continuous(breaks=seq(1,6,1))+
  scale_x_discrete(labels=c("1"="Credit card","2"="Cash","3"="No charge","4"="Dispute","5"="Unknown","6"="Voided trip"))+
  labs(title="PAYMENT TYPE",
       subtitle = "Comparing payment type and number of passengers",
       y="Number of passengers",
       x="Payment type")


#Cantidad pagada y tipo de pago 
#PLOT 14
ggplot(data=df_taxis,mapping=aes(y=total_amount,x=factor(payment_type)))+
  geom_violin(scale="area")+ #fill='aquamarine'
  theme_bw()+
  ylim(0,100)+
  scale_x_discrete(labels=c("1"="Credit card","2"="Cash","3"="No charge","4"="Dispute","5"="Unknown","6"="Voided trip"))+
  labs(title="PAYMENT TYPE",
       subtitle = "Distribution of payments",
       y="Amount to pay ($)",
       x="Payment type")


#Cantidad pagada y propina 
#PLOT 15
ggplot(data=df_taxis_paid,aes(x=total_amount,y=tip_amount))+
  geom_jitter(alpha=0.4)+  #color=lightpink
  theme_bw()+
  geom_smooth(method='lm')+
  labs(title="CORRELATION",
       subtitle = "Tips against the total amount to pay",
       x="Total amount to pay ($)",
       y="Tip ($)")

cor.test(df_taxis$total_amount, df_taxis$tip_amount, method = "pearson")

#PLOT 16
ggplot(data=df_taxis_paid,aes(x=trip_distance,y=tip_amount))+
  geom_point()+
  theme_bw()+
  scale_y_continuous(breaks=seq(0,100,20))+
  geom_smooth(method='lm')+
  labs(title="CORRELATION",
       subtitle = "Tips against the trip distance",
       x="Trip distance",
       y="Tip ($)")
  
cor.test(df_taxis$trip_distance, df_taxis$tip_amount, method = "pearson")

# MODELO: PREDECIR PRECIO DEL VIAJE
# sum(is.na(df_taxis$total_amount))
# sum(is.na(df_taxis$passenger_count))
# sum(is.na(df_taxis$trip_distance))
# sum(is.na(df_taxis$start_hour))
# sum(is.na(df_taxis$start_day))
# 
# df_taxis$passenger_count %>% drop_na(df_taxis$passenger_count)
# str(df_taxis$passenger_count)
# 
# df %>% filter(!is.na(df_taxis$passenger_count))
# sum(is.na(df_taxis$passenger_count))
# 
# modelo <- (lm(formula = total_amount ~ passenger_count + trip_distance +
#                 start_hour + start_day , data = df_taxis))
# summary(modelo)

