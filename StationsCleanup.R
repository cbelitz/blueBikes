library(tidyverse)

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
uniquestations %>% filter(., Name == "Sennott Park  Broadway at Norfolk Street")
uniquestations$Name <- sub("","",uniquestations$Name)

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


# Load the RIDE station ID into the CURRENT STATIONS so that we can join on RIDE ID




# Write the Unique Stations out to CSV
uniquecsvfile <- "data/gis/sept2019stns.csv"
write_csv(uniquestations, uniquecsvfile)


# Write Missing stations to CSV
missingcsvfile <- "data/eda/sept2019stns_miss_currentstns.csv"
write_csv(missing, missingcsvfile)
