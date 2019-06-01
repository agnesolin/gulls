# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~ CALCULATING TRIP FREQS DURING NIGHT AND DAY ~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


#### initial set-up ####

# clear the environment 
rm(list=ls())

# load libraries
library(suncalc)
library(rstudioapi)

# this opens a window allowing you to choose a directory where the input files are located (and where the output files will be stored)
setwd(selectDirectory())

# import all data
Islay = read.csv("all_info_Islay.csv")
Oronsay = read.csv("all_info_Oronsay.csv")
LadyIsle = read.csv("all_info_LadyIsle.csv")
Pladda = read.csv("all_info_Pladda.csv")

# add nest coordinates
nest_coordinates = read.csv("nest_coordinates.csv", sep = ";")


colonies = c("Islay", "Oronsay", "LadyIsle", "Pladda")

for(i in 1:length(colonies)){
  
  # this assigns the content of the individual colony dataframes to a dataframe called "data"
  # data gets re-assigned in each loop
  if(colonies[i] == "Islay") data = Islay        # loop round 1
  if(colonies[i] == "Oronsay") data = Oronsay    # loop round 2
  if(colonies[i] == "LadyIsle") data = LadyIsle  # loop round 3
  if(colonies[i] == "Pladda") data = Pladda      # loop round 4
  
  
  ### sorting out what is day and what is night ###
  
  # this merges the colony data with the nest co-ordinates by colony and bird id
  data = merge(data, nest_coordinates, all.x = T, all.y = F, by = c("Colony", "Bird"))
  
  # this makes a dataframe to use when calculating twilight times
  twilight = data.frame(date = as.Date(data$date_time), # current date
                              lat = data$nest_lat,      # latitude of nest
                              lon = data$nest_long)     # longitude of nest
  
  # this creates two new columns with the time of dawn and dusk based on the location and date
  data[,40:41] = getSunlightTimes(data = twilight, keep=c("dawn", "dusk"), tz="UTC")[,4:5]
  
  # here we make a new date column where whole night period get same date as previous day
  data$date_time = as.POSIXlt(data$date_time, tz = "UTC") # fixing format
  data$new_date = # first new column gets same value as the standard date column
    as.Date(data$date_time) 
  data$new_date[data$date_time < data$dawn] = # if before dawn, it gets assigned the date of the previous day
    data$new_date[data$date_time < data$dawn]-1
  
  # add night and day column
  data$day_night = 
    "night" # first gets assigned the value "night"
  data$day_night[data$date_time > data$dawn & data$date_time < data$dusk] = 
    "day" # if after dawn and before dusk it gets assigned "day"
  
  
  
  ### calculating trip frequencies
  
  # adding column defining whether resolution is sufficient (1 = good enough)
  data$res = 0
  if(colonies[i] == "Islay") data$res[data$interval< 11*60] = 1 # 11 min for Islay
  if(colonies[i] != "Islay") data$res[data$interval< 31*60] = 1 # 31 min for other colonies
  
  # adding column defining whether a trip was initialised at this time point (1 trip was initialised)
  data$init = 0
  data$init[
    !duplicated(data$trip_id) # equal to 1 if it is the first time a trip id appears (ie when trip was initialised)
    & !is.na(data$trip_id)] = 1
  
  
  # creating data frame with number of initalised trips day/night per date, and the total amount of time when resolution was high enough
  
  # creating a column that can be used to filter out samples just post dusk/dawn
  data$straddle = 0
  for(j in 1:(nrow(data)-1)){ 
    if(data$day_night[j] == "day" & data$day_night[j+1] == "night")  data$straddle[j+1] = 1 
    if(data$day_night[j] == "night" & data$day_night[j+1] == "day")  data$straddle[j+1] = 1 
  }
  
  
  # here we sum the number of trips initialised where resolution was high enough by bird id, date and day/night
  freqs = aggregate(data$init[data$res == 1 & data$straddle == 0], 
                    by = list(data$day_night[data$res == 1 & data$straddle == 0], data$new_date[data$res == 1 & data$straddle == 0],  data$Bird[data$res == 1 & data$straddle == 0]), 
                    FUN = sum)
  freqs = freqs[,c(3,2,1,4)] # making order more logical
  
  # giving it more sensible names
  names(freqs) = c("bird_id", "new_date", "day_night", "init_trips")
  

  # here we sum all sampling intervals where resolution was high enough by bird id, date and day/night = total sampling time
    freqs$sampling_time = aggregate(data$interval[data$res == 1 & data$straddle == 0], 
                                  by = list(data$day_night[data$res == 1 & data$straddle == 0], data$new_date[data$res == 1 & data$straddle == 0], data$Bird[data$res == 1 & data$straddle == 0]), 
                                  FUN = sum)$x # x just means that we only want the final column of the dataframe that aggregate produces
  
  

  # trip frequencies calculated as number of initalised trips per day/night per date divided by corresponding sampling time (converted to hours)
  freqs$trip_freq = freqs$init_trips/(freqs$sampling_time/60/60)
 
  
  
  ### adding average max distance and average trip length for each bird id, date and day/night combo ###
  
  data_dists = aggregate(data$max_dist[data$res == 1 & data$straddle == 0] , 
                         by = list(data$day_night[data$res == 1 & data$straddle == 0], data$new_date[data$res == 1 & data$straddle == 0], data$bird_id[data$res == 1 & data$straddle == 0]), 
                         FUN = mean)
  data_duration = aggregate(data$trip_duration[data$res == 1 & data$straddle == 0], 
                            by = list(data$day_night[data$res == 1 & data$straddle == 0], data$new_date[data$res == 1 & data$straddle == 0], data$bird_id[data$res == 1 & data$straddle == 0]), 
                            FUN = mean)
  
  names(data_dists) = c("day_night", "new_date", "bird_id", "avg_max_dist")
  names(data_duration) = c("day_night", "new_date", "bird_id", "avg_duration")
  
  data_dists = data_dists[!is.na(data_dists$avg_max_dist),]; data_duration = data_duration[!is.na(data_duration$avg_duration),]
  
  data_daynight = merge(freqs, data_dists, by = c("day_night", "new_date", "bird_id"), all = T)
  data_daynight = merge(data_daynight, data_duration, by = c("day_night", "new_date", "bird_id"), all = T)
  
  data_daynight = data_daynight[order(data_daynight$bird_id, data_daynight$new_date),]
  
  data_daynight$trip_freq[is.infinite(data_daynight$trip_freq)] = NA
  data_daynight$trip_freq[is.nan(data_daynight$trip_freq)] = NA
  
  # save as csv files
  write.csv(data_daynight, paste0(colonies[i],"_DayNight.csv"))

  
  # this code merges it all back together in case you want to link it with some other information
  # data = merge(data, freqs, 
  #              all.x = T, all.y = F,
  #              by = c("new_date","day_night", "bird_id"))
  # 
  
  }





