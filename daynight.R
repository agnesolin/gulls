# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~ CALCULATING TRIP FREQS DURING NIGHT AND DAY ~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


#### initial set-up ####

# clear the environment 
rm(list=ls())

# load libraries
library(suncalc)

# this opens a window allowing you to choose a directory where the input files are located (and where the output files will be stored)
setwd(selectDirectory())

# import all data
Islay <- read.csv("all_info_Islay.csv")
Oronsay <- read.csv("all_info_Oronsay.csv")
LadyIsle <- read.csv("all_info_LadyIsle.csv")
Pladda <- read.csv("all_info_Pladda.csv")


# add nest coordinates
nest_coordinates <- read.csv("nest_coordinates.csv", sep = ";")

Islay <- merge(Islay, nest_coordinates, all.x = T, all.y = F, by = c("Colony", "Bird"))
Oronsay <- merge(Oronsay, nest_coordinates, all.x = T, all.y = F, by = c("Colony", "Bird"))
LadyIsle <- merge(LadyIsle, nest_coordinates, all.x = T, all.y = F, by = c("Colony", "Bird"))
Pladda <- merge(Pladda, nest_coordinates, all.x = T, all.y = F, by = c("Colony", "Bird"))


# add times of dusk (end of civil twilight in the evening) and dawn (start of civil twilight in the morning)
twilight_Islay = data.frame(date = as.Date(Islay$date_time),
                            lat = Islay$nest_lat,
                            lon = Islay$nest_long)
twilight_Oronsay = data.frame(date = as.Date(Oronsay$date_time),
                            lat = Oronsay$nest_lat,
                            lon = Oronsay$nest_long)
twilight_LadyIsle = data.frame(date = as.Date(LadyIsle$date_time),
                            lat = LadyIsle$nest_lat,
                            lon = LadyIsle$nest_long)
twilight_Pladda = data.frame(date = as.Date(Pladda$date_time),
                            lat = Pladda$nest_lat,
                            lon = Pladda$nest_long)

Islay[,40:41] <- getSunlightTimes(data = twilight_Islay, keep=c("dawn", "dusk"), tz="UTC")[,4:5]
Oronsay[,40:41] <- getSunlightTimes(data = twilight_Oronsay, keep=c("dawn", "dusk"), tz="UTC")[,4:5]
LadyIsle[,40:41] <- getSunlightTimes(data = twilight_LadyIsle, keep=c("dawn", "dusk"), tz="UTC")[,4:5]
Pladda[,40:41] <- getSunlightTimes(data = twilight_Pladda, keep=c("dawn", "dusk"), tz="UTC")[,4:5]



# make new date column where whole night period get same date as previous day
Islay$date_time <- as.POSIXlt(Islay$date_time, tz = "UTC") 
Islay$new_date <- as.Date(Islay$date_time) 
Islay$new_date[Islay$date_time < Islay$dawn] = Islay$new_date[Islay$date_time < Islay$dawn]-1

Oronsay$date_time <- as.POSIXlt(Oronsay$date_time, tz = "UTC") 
Oronsay$new_date <- as.Date(Oronsay$date_time) 
Oronsay$new_date[Oronsay$date_time < Oronsay$dawn] = Oronsay$new_date[Oronsay$date_time < Oronsay$dawn]-1

LadyIsle$date_time <- as.POSIXlt(LadyIsle$date_time, tz = "UTC") 
LadyIsle$new_date <- as.Date(LadyIsle$date_time) 
LadyIsle$new_date[LadyIsle$date_time < LadyIsle$dawn] = LadyIsle$new_date[LadyIsle$date_time < LadyIsle$dawn]-1

Pladda$date_time <- as.POSIXlt(Pladda$date_time, tz = "UTC") 
Pladda$new_date <- as.Date(Pladda$date_time) 
Pladda$new_date[Pladda$date_time < Pladda$dawn] = Pladda$new_date[Pladda$date_time < Pladda$dawn]-1


# add night and day columns
Islay$day_night = "night"
Islay$day_night[Islay$date_time > Islay$dawn & Islay$date_time < Islay$dusk] = "day"

Oronsay$day_night = "night"
Oronsay$day_night[Oronsay$date_time > Oronsay$dawn & Oronsay$date_time < Oronsay$dusk] = "day"

LadyIsle$day_night = "night"
LadyIsle$day_night[LadyIsle$date_time > LadyIsle$dawn & LadyIsle$date_time < LadyIsle$dusk] = "day"

Pladda$day_night = "night"
Pladda$day_night[Pladda$date_time > Pladda$dawn & Pladda$date_time < Pladda$dusk] = "day"


# adding column defining whether resolution is sufficient (1 = good enough)
Islay$res = 0
Islay$res[Islay$interval< 11*60] = 1 # 11 min for Islay

Oronsay$res = 0
Oronsay$res[Oronsay$interval< 31*60] = 1 # 31 min

LadyIsle$res = 0
LadyIsle$res[LadyIsle$interval< 31*60] = 1 # 31 min

Pladda$res = 0
Pladda$res[Pladda$interval< 31*60] = 1 # 31 min


# adding column defining whether a trip was initialised at this time point (1 trip was initialised)
Islay$init = 0
Islay$init[!duplicated(Islay$trip_id) & !is.na(Islay$trip_id)] = 1

Oronsay$init = 0
Oronsay$init[!duplicated(Oronsay$trip_id) & !is.na(Oronsay$trip_id)] = 1

LadyIsle$init = 0
LadyIsle$init[!duplicated(LadyIsle$trip_id) & !is.na(LadyIsle$trip_id)] = 1

Pladda$init = 0
Pladda$init[!duplicated(Pladda$trip_id) & !is.na(Pladda$trip_id)] = 1


# creating data frame with number of initalised trips day/night per date, and the total amount of time when resolution was high enough
Islay_freqs = data.frame(new_date = aggregate(Islay$init, by = list(Islay$day_night, Islay$new_date), FUN = sum)$Group.2,
                         day_night = aggregate(Islay$init, by = list(Islay$day_night, Islay$new_date), FUN = sum)$Group.1,
                         init_trips = aggregate(Islay$init, by = list(Islay$day_night, Islay$new_date), FUN = sum)$x,
                         sampling_time = aggregate(Islay$res, by = list(Islay$day_night, Islay$new_date), FUN = sum)$x*10) # fixes every 10 min for Islay

Oronsay_freqs = data.frame(new_date = aggregate(Oronsay$init, by = list(Oronsay$day_night, Oronsay$new_date), FUN = sum)$Group.2,
                           day_night = aggregate(Oronsay$init, by = list(Oronsay$day_night, Oronsay$new_date), FUN = sum)$Group.1,
                         init_trips = aggregate(Oronsay$init, by = list(Oronsay$day_night, Oronsay$new_date), FUN = sum)$x,
                         sampling_time = aggregate(Oronsay$res, by = list(Oronsay$day_night, Oronsay$new_date), FUN = sum)$x*30) # fixes every 10 min

LadyIsle_freqs = data.frame(new_date = aggregate(LadyIsle$init, by = list(LadyIsle$day_night, LadyIsle$new_date), FUN = sum)$Group.2,
                            day_night = aggregate(LadyIsle$init, by = list(LadyIsle$day_night, LadyIsle$new_date), FUN = sum)$Group.1,
                         init_trips = aggregate(LadyIsle$init, by = list(LadyIsle$day_night, LadyIsle$new_date), FUN = sum)$x,
                         sampling_time = aggregate(LadyIsle$res, by = list(LadyIsle$day_night, LadyIsle$new_date), FUN = sum)$x*30) # fixes every 10 min

Pladda_freqs = data.frame(new_date = aggregate(Pladda$init, by = list(Pladda$day_night, Pladda$new_date), FUN = sum)$Group.2,
                          day_night = aggregate(Pladda$init, by = list(Pladda$day_night, Pladda$new_date), FUN = sum)$Group.1,
                         init_trips = aggregate(Pladda$init, by = list(Pladda$day_night, Pladda$new_date), FUN = sum)$x,
                         sampling_time = aggregate(Pladda$res, by = list(Pladda$day_night, Pladda$new_date), FUN = sum)$x*30) # fixes every 10 min


# trip frequencies calculated as number of initalised trips per day/night per date over corresponding sampling time (in hours)
Islay_freqs$trip_freq = Islay_freqs$init_trips/(Islay_freqs$sampling_time/60)
Oronsay_freqs$trip_freq = Oronsay_freqs$init_trips/(Oronsay_freqs$sampling_time/60)
LadyIsle_freqs$trip_freq = LadyIsle_freqs$init_trips/(LadyIsle_freqs$sampling_time/60)
Pladda_freqs$trip_freq = Pladda_freqs$init_trips/(Pladda_freqs$sampling_time/60)

# filter out estimates with short sampling times
min_sampling_time = 60*3

Islay_freqs = Islay_freqs[Islay_freqs$sampling_time > min_sampling_time,]
Oronsay_freqs = Oronsay_freqs[Oronsay_freqs$sampling_time > min_sampling_time,]
LadyIsle_freqs = LadyIsle_freqs[LadyIsle_freqs$sampling_time > min_sampling_time,]
Pladda_freqs = Pladda_freqs[Pladda_freqs$sampling_time > min_sampling_time,]


# plot of freq against day/night
plot(Islay_freqs$day_night, Islay_freqs$trip_freq)
plot(Oronsay_freqs$day_night, Oronsay_freqs$trip_freq)
plot(LadyIsle_freqs$day_night, LadyIsle_freqs$trip_freq)
plot(Pladda_freqs$day_night, Pladda_freqs$trip_freq)


# merge all back together
Islay = merge(Islay, Islay_freqs[,c(1,2,5)], 
              all.x = T, all.y = F,
              by = c("new_date","day_night"))

Oronsay = merge(Oronsay, Oronsay_freqs[,c(1,2,5)], 
              all.x = T, all.y = F,
              by = c("new_date","day_night"))

LadyIsle = merge(LadyIsle, LadyIsle_freqs[,c(1,2,5)], 
                all.x = T, all.y = F,
                by = c("new_date","day_night"))

Pladda = merge(Pladda, Pladda_freqs[,c(1,2,5)], 
                all.x = T, all.y = F,
                by = c("new_date","day_night"))


# save as csv files
write.csv(Islay, "Islay_DayNightFreq.csv")
write.csv(Oronsay, "Oronsay_DayNightFreq.csv")
write.csv(LadyIsle, "LadyIsle_DayNightFreq.csv")
write.csv(Pladda, "Pladda_DayNightFreq.csv")


