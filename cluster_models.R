library(e1071)
library(rmse)

# set factors
trips_per_day_09_19$weekday = as.factor(trips_per_day_09_19$weekday)
trips_per_day_09_19$District = as.factor(trips_per_day_09_19$District)
trips_per_day_09_19$Public = as.factor(trips_per_day_09_19$Public)

# remove missing values
clean_trips = na.omit(trips_per_day_09_19)

# pcr
pr.trips = prcomp(clean_trips)
