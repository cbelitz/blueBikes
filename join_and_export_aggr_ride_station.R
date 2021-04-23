# JOINING STATION DATA TO AGGREGATE TRIP DATA

# PURPOSE: This script will join station attribute data to the aggregated ride data
#         so station info can be used in analysis.

# PREREQUISITES: Run trips_per_day_by_station.r first
#                This script relies on having agg_trips in the environment
#                --Alternatively, we could alter the trips_per_day to write agg_trips 
#                  out to a dataset, and read that in.

#install.packages("dplyr") # the tidyverse version of plyr.
# dplyr included in the call below, but be aware that loading dplyr will crap on some plyr functions, so 
# running trips_per_day_by_station.r (which uses plyr) will fail in the same session after loading tidyverse. :(

library(tidyverse)

##################
### READ FILES ###
##################

# READ in the trip data
# agg_trips already in memory; see note above

# READ in the station data
stations <- read.csv("data/stations_09_19.csv")

# READ in the weather data, forcing class so Date is Date
weather <- read.csv("data/wunderground_09_19.csv", colClasses = c("Date","integer","numeric","integer","integer","numeric","integer","numeric"))


#################
### JOIN DATA ###
#################

# JOIN the station data to the aggregated trip data
dailytrips <- left_join(agg_trips, stations, by = "id")

# JOIN the weather data
dailytrips <- left_join(dailytrips, weather, by = c("date" = "Date"))

# VERIFICATION-LITE :)
# Look at the data, checking for NAs. Only in Total.docks, which makes sense, because docks are from the provided csv,
#   and not all stations in the Sept ride data were present in the provided csv.
summary(dailytrips)

# Sum of total flow: I would expect this to be around 0, but maybe not exactly due to the cleaning/removal 
#   of some long rides. Looks like it is exactly 0.
sum(dailytrips$flow)

# PREPARE to export the data
# Remove the CSV station label (using ID instead) and geometry fields
dailytrips.predictors <- dailytrips[,-c(8,12)]


# WRITE the aggregated ride data -- with station attribute data -- out to CSV
csvfile <- "data/trips_per_day_09_19.csv"
write_csv(dailytrips.predictors, csvfile)
