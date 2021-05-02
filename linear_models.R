# Linear Models


library(tidyverse)


# READ in the trips by station, day data
tripClasses <- c("factor", "Date", "integer", "integer", "integer", "factor", "factor", "factor", "factor", "integer", "numeric", "numeric", "integer", "numeric", "integer", "integer", "numeric","integer","numeric")
trips <- read.csv("data/trips_per_day_09_19.csv", colClasses = tripClasses)


# Calc the Z score of the trips per day, by monthly average for that station
# The purpose of this (I think) is to know, for a given station, how many trips is that relative to the std dev of that station
# Because it is hard to compare total trips when certain stations have SOOOO many trips
trips$ztrips <- ave(trips$total.start, trips$id, FUN=scale)

# Categorize weekday/weekend
trips$weekdayend <- ifelse(trips$weekday %in% c("Sunday","Saturday"), "weekend", "weekday")

# Don't wanna be typing "trips$" all day :)
attach(trips)

# First, some EDA

summary(trips)

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
daily <- trips %>% group_by(date, weekday, weekdayend, Precipitation, TempAvg) %>% summarize(starts = sum(total.start), ends = sum(total.end))
dailyz <- trips %>% group_by(date, weekday, weekdayend, Precipitation, TempAvg) %>% summarize(avgz = mean(ztrips))

# PLOT: Daily Starts (Rides) by Date
plot(daily$date,daily$starts)
ggplot(data = daily, aes(date, starts, colour = weekdayend)) +
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 0, size = 1) +
  ggtitle("Total Rides by Date") +
  xlab("Date") +
  ylab("Total Rides")

# PLOT: Average Station Z-Score by Date
plot(dailyz$date,dailyz$avgz)
ggplot(data = dailyz, aes(date, avgz, colour = weekdayend)) +
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 0, size = 1) +
  ggtitle("Average Station Z-Score by Date")
  xlab("Date") +
  ylab("Avg Station Z-score")
  
  ## ^^ I feel like I may be on to something here. The latter part of the month shows some difference between Total Rides
  ## and the Avg Z-score. I think that on weekends, there may be a significant drop in rides, but not the average z-score,
  ## indicating that it may be a drop in the heavy-usage station on the weekend, which shows up in the total number but not
  ## the avg Z-score. If that makes sense...
  
  ## Final note: Labor Day was way down

# Does the Precipitation matter?

  ## We don't have a large number of rainy days (small sample size)
  ## When we run this on the system-wide daily summary, we get a positive relationship. Which says less that people
  ## want to ride in the rain, and more that they didn't ride on some nice days.

lm_precip <- lm(daily$starts ~ daily$Precipitation)
summary(lm_precip)

plot(daily$Precipitation,daily$starts)
abline(lm_precip)
plot(lm_precip)


# Does the Average Temperature matter?

  ## Doesn't seem to (visually). Also thinking, in September the TempAvg ran from 57 to 78. All rideable temperatures.
  ## We may not notice a gradient on this dataset. Should we be looking at the whole year?
plot(daily$TempAvg,daily$starts)
plot(dailyz$TempAvg,dailyz$avgz)

# What about distance to T Stops and Bike Lanes?

plot(tstopdist,total.start) # Is there overlap? Like, T-Stops are designed around more people. More people = more potential riders
plot(bikelanedist,total.start)


lm1 <- lm(total.start ~ id + weekday + TempAvg, data = trips)
summary(lm1) 
