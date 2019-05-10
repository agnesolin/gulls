# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~ EXTRACTING TRIP CHARACTERISTICS AND FILTERING OUT LOW-RES DATA ~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


#### initial set-up ####

# clear the environment 
rm(list=ls())

# load libraries
library(rstudioapi)
library(rworldmap)
library(rworldxtra)
library(sp)
library(geosphere)
library(lubridate)


# this opens a window allowing you to choose a directory where the input files are located (and where the output files will be stored)
setwd(selectDirectory())

# select which colony to use (the other ones should be hashed out)
# colony <- "Islay";         
colony <- "LadyIsle";      
# colony <- "Oronsay";    
# colony <- "Pladda";        


# import data (the data file imported depends on which colony you have chosen)
gulls <- read.csv(paste0(colony, "AllNo0.csv"), header = T)

# create a proper date/time column
gulls$date_time <- as.POSIXct(paste(paste0(20,gulls$Year), gulls$Month, gulls$Day, gulls$Hour, gulls$Minute, gulls$Second), 
                              format = "%Y %m %d %H %M %S", tz="GMT")


# make sure data frame is sorted by bird, then by date_time
gulls <- gulls[order(gulls$Bird, gulls$date_time),]

str(gulls)

#### setting parameters ####

buffer <- 0.3 # this is where you set the size of the buffer you want around the colony co-ordinates (if outside buffer == trip)

min_res <- 31 # this is the time interval needed for all fixes in a trip (e.g. avg time back at colony) - otherwise whole trip is discarded
min_res_nest <- 31 # this is the time interval needed in order for nest attendance to be calculated (if resolution is too low, no nest attendance lengths are calculated)

# this is to set time period you are interested in (can use finer cut-off points)
min_month <- 4
max_month <- 7



#### plot unfiltered data ####

newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(floor(min(gulls$Long)), ceiling(max(gulls$Long))), ylim = c(floor(min(gulls$Lat)), ceiling(max(gulls$Lat))), 
     xlab = "Recorded Locations")

points(gulls$Long, gulls$Lat, # plots each location
       cex = 1, # adjust size of points
       pch = 19) # adjust type of point



#### filtering of data ####

# remove points recorded at equator
gulls <- gulls[gulls$Lat > 50,]

# Oronsay, Black Isle (Bird 12703)
# only if Bird ID 12703 from Oronsay is run
# gulls <- gulls[gulls$Bird == 12703,]

# Oronsay, Carn (all other Birds)
gulls <- gulls[gulls$Bird != 12703,]

# remove odd locations from Lady Isle and Oronsay (ideally we want to come up with a rule for how these are removed)
if(colony == "LadyIsle") gulls <- gulls[gulls$Lat < 56,]; gulls <- gulls[gulls$Long < -3,]
if(colony == "Oronsay") gulls <- gulls[gulls$Lat < 57,]; gulls <- gulls[gulls$Long > -8,]



#### initial plotting ####

# this section plots filtered data, colour-coded by no of satellites, bird id and year

# change margins of plot so that we can fit a legend
par(mar=c(5.1, 4.1, 4.1, 8.1))

# separate colours by no of satellites
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(floor(min(gulls$Long)), ceiling(max(gulls$Long))), ylim = c(floor(min(gulls$Lat)), ceiling(max(gulls$Lat))), 
     xlab = "Recorded Locations")

cols <- palette()[1:length(unique(gulls$Satellites))]

points(gulls$Long, gulls$Lat, # plots each location
       col = cols[as.numeric(as.factor(rank(gulls$Satellites)))], # colour codes it by no of satellites
       cex = 1, # adjust size of points
       pch = 19) # adjust type of point

legend("right", 
       inset=c(-0.2,0), 
       legend = sort(unique(gulls$Satellites)), 
       pch = c(19), 
       col = cols, 
       bty = "n", 
       xpd = T,
       title = "No Satellites")

# selecting only breeding season (April to July)
gulls <- gulls[gulls$Month > 3 & gulls$Month < 8,]

# separate colours by bird id
newmap <- getMap(resolution = "high")
plot(newmap, xlim = c(floor(min(gulls$Long)), ceiling(max(gulls$Long))), ylim = c(floor(min(gulls$Lat)), ceiling(max(gulls$Lat))), 
     xlab = "Recorded Locations")

cols <- palette()[1:length(unique(gulls$Bird))]

points(gulls$Long, gulls$Lat, # plots each location
       col = cols[as.numeric(as.factor(rank(gulls$Bird)))], # colour codes it by bird id
       cex = 1, # adjust size of points
       pch = 19) # adjust type of point

legend("right", 
       inset=c(-0.2,0), 
       legend = unique(gulls$Bird), 
       pch = c(19), 
       col = cols, 
       bty = "n", 
       xpd = T,
       title = "Bird ID")



# separate colours by year 
newmap <- getMap(resolution = "high")
plot(newmap, xlim = c(floor(min(gulls$Long)), ceiling(max(gulls$Long))), ylim = c(floor(min(gulls$Lat)), ceiling(max(gulls$Lat))), 
     xlab = "Recorded Locations")

cols <- palette()[1:length(unique(gulls$Year))]

points(gulls$Long, gulls$Lat, # plots each location
       col = cols[as.numeric(as.factor(rank(gulls$Year)))], # colour codes it by year (clunky way of extracting rank of each year i.e. 2014 = 1, 2015 = 2 etc, so that values can be extracted from colour vector)
       cex = 1, # adjust size of points
       pch = 19) # adjust type of point

legend("right", 
       inset=c(-0.2,0), 
       legend = unique(gulls$Year), 
       pch = c(19), 
       col = cols, 
       bty = "n", 
       xpd = T,
       title = "Year")



# re-set margins
par(mar=c(5.1, 4.1, 4.1, 2.1))




#### calculate distance to colony for each fix and time interval between fixes ####

# add nest-coordinates
nest_coordinates <- read.csv("nest_coordinates.csv", header = T, sep = ";")

# create spatial object for all recorded locations
locs = SpatialPoints(coords = cbind(gulls$Long, gulls$Lat), proj4string=CRS("+proj=longlat +datum=WGS84"))

# calculate distance for each recording to individual-specific nest coordinates
for(i in 1:length(unique(gulls$Bird))){
  C = SpatialPoints(coords = cbind(nest_coordinates$nest_long[nest_coordinates$Bird == unique(gulls$Bird)[i]], nest_coordinates$nest_lat[nest_coordinates$Bird == unique(gulls$Bird)[i]]), proj4string=CRS("+proj=longlat +datum=WGS84"))
  gulls$distance[gulls$Bird == unique(gulls$Bird)[i]] = spDistsN1(locs[gulls$Bird == unique(gulls$Bird)[i]], C, longlat = T) # calculates distance from colony in km
}


# calculate time intervals between fixes in seconds (this will be inaccurate for first record of each bird - deal with this later)
gulls$interval = c(NA, difftime(gulls$date_time[2:length(gulls$date_time)], gulls$date_time[1:length(gulls$date_time)-1])) # NA first because no previous recordings (can't calculate interval)


# there are some odd duplicates in the data - might want to look into that
gulls <- gulls[!duplicated(gulls[c("Bird", "date_time")]),] # removes duplicates based on bird id and date_time



#### define trips and calculate characteristics separately for each bird ####

birds <- unique(gulls$Bird) # makes a list of all bird ids

trips <- data.frame(NULL) # sets up empty data frame to store trip characteristics summary
gulls_new <- data.frame(NULL) # sets up empty data frame that include both fixes and trip characteristics


for(b in 1:length(birds)) { # this loops through all the individuals birds so that we can calculate trip characterstics separately
  
  gulls_ind <- gulls[gulls$Bird == birds[b],] # filters out locations for focal bird
  
  gulls_ind$interval[1] <- NA # sets first time interval to NA (as there is no previous recording to calculate a time difference for)
  
  
  #### define trips ####
  
  # plot all trips with buffer line
  plot(x = gulls_ind$date_time, y = gulls_ind$distance, type = "l")
  abline(h = buffer, col = "red", lty = 2)
  
  # sets up column to store trip id
  gulls_ind$trip <- NA
  gulls_ind$trip[gulls_ind$distance <= buffer] <- 0 # trip = 0 if back at colony
  
  
  # remove all records before first recording inside buffer
  gulls_ind <- gulls_ind[min((1:nrow(gulls_ind))[gulls_ind$trip == 0], na.rm = T):nrow(gulls_ind), ]
  
  # here we loop through all fixes and when bird is outside buffer gulls_ind$trip is given a number 
  # each fix until the bird is back at the colony is given the same number
  # gulls_ind$trip thus have value 0 when at colony and otherwise the trip id
  
  j = 0
  
  for(i in 2:length(gulls_ind$trip)) {
    
    if(is.na(gulls_ind$trip[i]) & gulls_ind$trip[i-1] == 0) {
      j <- j + 1
      gulls_ind$trip[i] <- j }
    
    if(is.na(gulls_ind$trip[i]) & gulls_ind$trip[i-1] != 0) {
      gulls_ind$trip[i] <- j }
  }
  

  
  #### calculating trip characteristics and filter out trips that are low-res #### 
  
  # calculate angle between subsequent fixes
  gulls_ind$angle <- bearing(cbind(gulls_ind$Long, gulls_ind$Lat))
  
  # calculate speed between fixes (m/s)
  gulls_ind$speed <- c(NA, abs(gulls_ind$distance[2:nrow(gulls_ind)]-gulls_ind$distance[1:(nrow(gulls_ind)-1)])*1000/gulls_ind$interval[2:nrow(gulls_ind)])
  
  # setting up variables
  dest_angle <- rep(NA, max(gulls_ind$trip))
  dest_loc <- cbind(rep(NA, max(gulls_ind$trip)), rep(NA, max(gulls_ind$trip)))
  departing_angle <- rep(NA, max(gulls_ind$trip))
  
  start_time <- rep(gulls_ind$date_time[1], max(gulls_ind$trip)) # this is just to get the right format
  end_time <- rep(gulls_ind$date_time[1], max(gulls_ind$trip)) # this is just to get the right format
  
  nest_attendance <- rep(NA, max(gulls_ind$trip)) 
  
  # loops through all trip ids and calculate statistics OR set them to NA when res is too low (as based on min res defined at beginning of script)
  for(i in 1:max(gulls_ind$trip))  {
    
    
    if(max(gulls_ind$interval[min(which(gulls_ind$trip == i)):(max(which(gulls_ind$trip == i))+1)], na.rm = T)/60 > min_res) { # all get NA when res is too low ie trip will be ignored
      
      dest_angle[i] <- NA; dest_loc[i,1] <- NA; dest_loc[i,2] <- NA; 
      departing_angle[i] <- NA; start_time[i] <- NA; end_time[i] <- NA }
    
    
    else {  # when res is high enough trip characteristics are calculated
      
      # last fix inside buffer
      start_lat <- gulls_ind$Lat[min(which(gulls_ind$trip == i)) -1]
      start_long <- gulls_ind$Long[min(which(gulls_ind$trip == i)) -1]
      
      # fix in trip at longest distance from colony
      max_lat <- gulls_ind$Lat[gulls_ind$trip == i][aggregate(gulls_ind$distance, list(gulls_ind$trip), FUN = which.max)[i+1,2]]
      max_long <- gulls_ind$Long[gulls_ind$trip == i][aggregate(gulls_ind$distance, list(gulls_ind$trip), FUN = which.max)[i+1,2]]
      
      # angle to and location of final destination (max distance)
      dest_angle[i] <- bearing(cbind(c(start_long, max_long), c(start_lat, max_lat)))[1]
      dest_loc[i,1] <- max_lat
      dest_loc[i,2] <- max_long
      
      # angle between last fix in colony and first fix outside colony in a trip
      departing_angle[i] <- gulls_ind$angle[min(which(gulls_ind$trip == i))-1]
      
      # assume starting point is first fix outside buffer - interval since last fix in buffer/2
      start_time[i] <- gulls_ind$date_time[min(which(gulls_ind$trip == i))] - 
        gulls_ind$interval[min(which(gulls_ind$trip == i))]/2
      
      # assume ending point is last fix outside buffer + interval until next fix in buffer/2
      end_time[i] <- gulls_ind$date_time[max(which(gulls_ind$trip == i))] +
        gulls_ind$interval[max(which(gulls_ind$trip == i))+1]/2
      
      # nest attendance
      if(i > 1) if(!is.na(end_time[i-1]) & !is.na(start_time[i])) if(sum(gulls_ind$interval[gulls_ind$date_time <= start_time[i] & gulls_ind$date_time > end_time[i-1]] > min_res_nest*60) == 0) nest_attendance[i] <- difftime(start_time[i], end_time[i-1], units = "mins")
        

      
    }
    
  }
  
  # trip duration is just end time - start time
  trip_duration <- difftime(end_time, start_time, units = "mins") # trip length in mins
  
  # maximum distance recorded during trip
  max_dist <- aggregate(gulls_ind$distance[gulls_ind$trip != 0], list(gulls_ind$trip[gulls_ind$trip != 0]), FUN = max)
  
  # maximum speed recorded during trip
  max_speed <- aggregate(gulls_ind$speed[gulls_ind$trip != 0], list(gulls_ind$trip[gulls_ind$trip != 0]), FUN = max)
  
  
  #### collate all trips into data frame ####
  
  # only current trips
  trips_temp <- data.frame(bird_id = rep(birds[b], max(gulls_ind$trip)), 
                           colony = rep(gulls_ind$Colony[1], max(gulls_ind$trip)), 
                           trip = 1:max(gulls_ind$trip),
                           trip_id = paste(1:max(gulls_ind$trip), gulls_ind$Bird[1], sep = "_"), 
                           start_time, 
                           end_time, 
                           nest_attendance,
                           trip_duration, 
                           max_dist = max_dist[,2], 
                           dest_lat = dest_loc[,1],
                           dest_long = dest_loc[,2],
                           departing_angle, 
                           dest_angle,
                           max_speed = max_speed[,2])
  
  
  # remove first trip if it did not start back at the colony
  if(gulls_ind$trip[1] != 0) trips_temp <- trips_temp[-1,]
  
  # remove last trip if it did not finish back at the colony
  if(gulls_ind$trip[nrow(gulls_ind)] != 0) trips_temp <- trips_temp[-nrow(trips_temp),]
  
  # to create a data frame with trips for all birds
  trips <- rbind(trips, trips_temp)
  
  # this merges trip characteristics with all fixes and collates for all individuals
  gulls_new_temp <- merge(gulls_ind, trips_temp, by = "trip", all = T)
  gulls_new_temp <- gulls_new_temp[order(gulls_new_temp$date_time),]
  gulls_new <- rbind(gulls_new, gulls_new_temp)
  
}


# remove all records that are outside time period and where res was too low (this is captured by start_time being NA)
trips <- trips[!is.na(month(trips$start_time)) & month(trips$start_time) >= min_month & month(trips$end_time) <= max_month,]



#### checking values ####
summary(trips) 
min(trips$trip_duration) 
max(trips$trip_duration) 
table(gulls$interval)
table(trips$nest_attendance)

#### saving csv file of trips and of all recordings merged with trip charachterstics and includes colony name
write.csv(trips, paste0("trips_", colony, ".csv")) # this saves the file in the same directory 
write.csv(gulls_new, paste0("all_info_", colony, ".csv")) # this saves the file in the same directory and includes colony name


