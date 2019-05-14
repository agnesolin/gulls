#############################################
#### Claculate Lunar Illumination        ####
#############################################

# after Lazaridis 2014

#### initial set-up ####

# clear the environment 
rm(list=ls())

# load libraries
install.packages("lunar")

# import data 
setwd("C:/Users/rn6f/Dropbox/RESEARCH/Projects/Pathtrack/Trackingfiles") #Ruedi's desktop directory
Islay <- read.csv("all_info_Islay.csv")
nest_coordinates <- read.csv("nest_coordinates.csv", sep = ";")

Islay <- merge(Islay, nest_coordinates, all.x = T, all.y = F, by = c("Colony", "Bird"))

Islay$Datum <- as.POSIXct(paste(Islay$Year, Islay$Month, Islay$Day), 
                          format = "%y %m %d", tz="GMT")

Islay$dectime = (Islay$Hour + (Islay$Minute/60))

# add fraction of the moon disk that is illuminated at the start of a trip
moonphase_Islay = data.frame(date = as.Date(Islay$date_time),
                            lat = Islay$nest_lat,
                            lon = Islay$nest_long)

Islay[,42] <- lunar.illumination(as.Date(Islay$Datum), 13 - Islay$dectime)

Islay$moonphase <- Islay$V42
Islay$V42 <- NULL

# check first value in datafram
lunar.illumination(as.Date("2014-05-28"), -2.8)
