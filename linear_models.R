# Linear Models


library(tidyverse)


# READ in the trips by station, day data
tripClasses <- c("factor", "Date", "integer", "integer", "integer", "factor", "factor", "factor", "factor", "integer", "numeric", "numeric", "integer", "numeric", "integer", "integer", "numeric","integer","numeric")
trips <- read.csv("data/trips_per_day_09_19.csv", colClasses = tripClasses)


# Calc the Z score of the trips per day, by monthly average for that station
trips$ztrips <- ave(trips$total.start, trips$id, FUN=scale)

attach(trips)

# First, some EDA

# What is the distribution of flow across stations?
hist(flow, breaks = 50)

# What is the distribution of total rides per station per day?
hist(total.start, breaks = 50)
hist(ztrips, breaks = 50)

# What is the distribution of rides per station for the month? (Ignore the id plot)
trips %>% 
  group_by(id) %>% 
  summarize(Trips = sum(total.start)) %>%
  boxplot()


# Which days are busiest?
table(weekday)
plot(table(weekday))
# These should all be about the same, but Sunday and Monday have the highest counts.
# Why? There are 5 of each in this month, vs 4 of each for the other days.

# Total trips system-wide by day
daily <- trips %>% group_by(date, weekday, Precipitation, TempAvg) %>% summarize(starts = sum(total.start), ends = sum(total.end))
dailyz <- trips %>% group_by(date, weekday, Precipitation, TempAvg) %>% summarize(avgz = ave(ztrips))
plot(date,dailyz$avgz)

# Does the Precipitation matter?
plot(daily$Precipitation,daily$starts)
lm_precip <- lm(ztrips ~ Precipitation)
summary(lm_precip)
plot(lm_precip)

# Does the Average Temperature matter?
plot(daily$TempAvg,daily$starts)


plot(tstopdist,total.start) # Is there overlap? Like, T-Stops are designed around more people. More people = more potential riders
plot(bikelanedist,total.start)


lm1 <- lm(total.start ~ id + weekday + TempAvg, data = trips)
summary(lm1) 
