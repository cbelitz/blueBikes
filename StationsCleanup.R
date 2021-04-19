#install.packages("tidyverse") # For general data purposes
#install.packages("sf") # For geospatial

library(tidyverse)
library(sf)

###################
### IMPORT DATA ###
###################

## RIDE DATA
# Ride data CSV location
stationfile <- "data/blueBikes/201909-bluebikes-tripdata.csv"

# Read in original data to dataframe from CSV
ridestations <- read.csv(stationfile)
attach(ridestations)


## STATION DATA
# Current Stations from the CSV file provided by Bluebikes
currentstationfile <- "data/blueBikes/current_bluebikes_stations.csv"

# Read current stations to dataframe from CSV
currentstations <- read.csv(currentstationfile)

######################
### TRANSFORM DATA ###
######################

# Put all station info (start and end) into same columns
stackstations <- data.frame(c(start.station.id, end.station.id), c(start.station.name,end.station.name), c(start.station.latitude,end.station.latitude),c(start.station.longitude,end.station.longitude))

# Find list of unique stations based on the ride data
uniquestations <- unique(stackstations)
names(uniquestations) <- c("id", "Name", "Latitude", "Longitude")


################
### CLEANING ###
################

# Find stations in the Ride data that are missing in the Current Stations CSV
missing_beforeclean <- anti_join(uniquestations, currentstations, by = "Name")

# Errors in Station Name
# sub (RIDE name, STATION CSV)
#uniquestations$Name <- sub("","",uniquestations$Name)

uniquestations$Name <- sub("Allston Green District - Griggs St at Commonwealth Ave","Commonwealth Ave at Griggs St",uniquestations$Name)
uniquestations$Name <- sub("Big Papi Station","Lansdowne T Stop",uniquestations$Name)
uniquestations$Name <- sub("Congress St at North St","Congress St at Boston City Hall",uniquestations$Name)
uniquestations$Name <- sub("Cummins at American Legion","American Legion Hwy at Canterbury St",uniquestations$Name)
uniquestations$Name <- sub("Dudley Square - Dudley St at Warren St","Dudley Square - Bolling Building",uniquestations$Name)
uniquestations$Name <- sub("Franklin Park - Seaver St at Humbolt Ave","Franklin Park - Seaver St at Humboldt Ave",uniquestations$Name)
uniquestations$Name <- sub("Harvard University Gund Hall at Quincy St / Kirkland S","Harvard University Gund Hall at Quincy St / Kirkland St",uniquestations$Name)
uniquestations$Name <- sub("Mobile Temporary Station","MIT Pacific St at Purrington St",uniquestations$Name)
uniquestations$Name <- sub("Prudential Center - Belvedere St","Prudential Center - 101 Huntington Ave",uniquestations$Name)
uniquestations$Name <- sub("Sennott Park  Broadway at Norfolk Street","Sennott Park Broadway at Norfolk Street",uniquestations$Name)
uniquestations$Name <- sub("Upham's Corner","Uphams Corner",uniquestations$Name)
uniquestations$Name <- sub("Upham's Corner T Stop - Magnolia St at Dudley St","Uphams Corner T Stop - Magnolia St at Dudley St",uniquestations$Name)

# Find stations in the Ride data that are missing in the Current Stations CSV
missing_afterclean <- anti_join(uniquestations, currentstations, by = "Name")


# OPTIONAL Write Missing stations to CSV for further investigation
#missingcsvfile <- "data/eda/sept2019stns_miss_currentstns.csv"
#write_csv(missing_afterclean, missingcsvfile)


### JOIN DATA FOR STATION DETAILS ###
# Join the provided Station data for additional attributes (District, Public, Total.docks, minus the lat/longs)
stations <- left_join(uniquestations, currentstations[,c(1:2,5:7)], by = "Name")


######################
### CALC DISTANCES ###
######################

# Using Massachusetts State Plane meters, EPSG: 26986
# https://www.mass.gov/info-details/overview-of-massgis-data#spatial-reference-of-massgis-data-

# Create Point features from Lat/Long
temp <- st_as_sf(stations, coords=c("Longitude", "Latitude"), crs = 4326, agr = "constant")
#st_is_longlat(temp)

# Reproject features to Mass State Plane for measuring distance
stations <- st_transform(temp, crs = 26986)

### LOAD BIKELANES AND TSTOPS ###
bikelanes <- read_sf("data/gis/bikelanes_ma.shp")
st_crs(bikelanes) # check projection

tstops <- read_sf("data/gis/tstops.shp")
st_crs(tstops)

### Find the nearest feature for each bike station, then calculate the distance to that feature
nearest_bikelane_id <- st_nearest_feature(stations, bikelanes)
stations$bikelanedist <- st_distance(stations, bikelanes[nearest_bikelane_id,], by_element = TRUE)

nearest_tstop_id <- st_nearest_feature(stations, tstops)
stations$tstopdist <- st_distance(stations, tstops[nearest_tstop_id,], by_element = TRUE)

### COMMENTING THIS SECTION OUT, because we may not even need this data.
### Find the nearest station. Comparing station to station, remove current one: https://stackoverflow.com/questions/49200458/find-nearest-features-using-sf-in-r
#closest <- numeric()
#for(i in seq_len(nrow(stations))){
#  #closest[[i]] <- stations[which.min(st_distance(stations[-i,], stations[i,])),]
#  closest[i] <- st_distance(stations[i,], stations[-i,])
#}

#stations$stndist <- closest

# Write a shapefile for data verification
write_sf(stations, "data/gis/TESTstations.shp")

# Write the Stations out to CSV
csvfile <- "data/sept2019stns.csv"
write_csv(stations, csvfile)



