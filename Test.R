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


if(!require("read.csv")) {
  install.packages("read.csv")
  library("read.csv")
}


### LECTURA DEL ARCHIVO ###
df_taxis=read.csv(file="2019_Yellow_Taxi_Trip_Data.csv",nrows=100000)
str(df_taxis)

