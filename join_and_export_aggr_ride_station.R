# JOINING STATION DATA TO AGGREGATE TRIP DATA

# PURPOSE: This script will join station attribute data to the aggregated ride data
#         so station info can be used in analysis.

# PREREQUISITES: Run trips_per_day_by_station.r first
#                This script relies on having agg_trips in the environment
#                --Alternatively, we could alter the trips_per_day to write agg_trips 
#                  out to a dataset, and read that in.

#install.packages("dplyr") # the tidyverse version of plyr.

library(tidyverse)

# agg_trips already in memory
stations <- read.csv("data/stations_09_19.csv")

dailytrips <- left_join(agg_trips, stations, by = "id")

# Look at the data, checking for NAs. Only in Total.docks, which makes sense, because docks are from the provided csv,
#   and not all stations in the Sept ride data were present in the provided csv.
summary(dailytrips)

# Sum of total flow: I would expect this to be around 0, but maybe not exactly due to the cleaning/removal 
#   of some long rides. Looks like it is exactly 0.
sum(dailytrips$flow)

# Remove the CSV station label (using ID instead) and geometry fields
dailytrips.predictors <- dailytrips[,-c(8,12)]


# Write the Stations out to CSV
csvfile <- "data/trips_per_day_09_19.csv"
write_csv(dailytrips.predictors, csvfile)
