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

df_taxis = read.csv(file="2019_Yellow_Taxi_Trip_Data.csv", nrows=300000)
str(df_taxis)

#

sum(is.na(df_taxis$passenger_count))
sum(is.na(df_taxis$passenger_count))
sum(is.na(df_taxis$trip_distance))


df_taxis=na.omit(df_taxis)
sum(is.na(df_taxis$passenger_count))
sum(is.na(df_taxis$trip_distance))

date_start = as.numeric(df_taxis$tpep_pickup_datetime)



hist(x=date_start)

