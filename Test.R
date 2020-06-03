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

<<<<<<< HEAD

if(!require("read.csv")) {
  install.packages("read.csv")
  library("read.csv")
}


### LECTURA DEL ARCHIVO ###
df_taxis=read.csv(file="2019_Yellow_Taxi_Trip_Data.csv",nrows=100000)
str(df_taxis)

=======
df_taxis = read.csv(file="2019_Yellow_Taxi_Trip_Data.csv", nrows=300000)

str(df_taxis)
>>>>>>> e58cfdbfdb1f48a2ecb3c78e5e311e5d9043aec5
