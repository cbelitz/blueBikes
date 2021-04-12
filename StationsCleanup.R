library(tidyverse)

# Ride data CSV location
stationfile <- "data/blueBikes/201909-bluebikes-tripdata.csv"

# Read in original data to dataframe from CSV
ridestations <- read.csv(stationfile)
attach(ridestations)

# Put all station info (start and end) into same columns
stackstations <- data.frame(c(start.station.id, end.station.id), c(start.station.name,end.station.name), c(start.station.latitude,end.station.latitude),c(start.station.longitude,end.station.longitude))

# Find list of unique stations
uniquestations <- unique(stackstations)
names(uniquestations) <- c("id", "Name", "Latitude", "Longitude")

# Current Stations
currentstationfile <- "data/blueBikes/current_bluebikes_stations.csv"

# Read current stations to dataframe from CSV
currentstations <- read.csv(currentstationfile)

# Find stations in the Ride data that are missing in the Current Stations CSV
missing <- anti_join(uniquestations, currentstations, by = "Name")


# Write the Unique Stations out to CSV


# Write Missing stations to CSV
missingcsvfile <- "data/eda/sept2019stns_miss_currentstns.csv"
write_csv(missing, missingcsvfile)
