# Linear Models


library(tidyverse)


# READ in the trips by station, day data
trips <- read.csv("data/trips_per_day_09_19.csv")
attach(trips)

# First, some EDA

hist(flow, breaks = 50)

table(weekday)
plot(table(weekday))
# These should all be about the same, but Sunday and Monday have the highest counts.
# Why? There are 5 of each in this month, vs 4 of each for the other days.

