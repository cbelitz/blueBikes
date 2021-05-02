# Linear Models


library(tidyverse)


# READ in the trips by station, day data
# tripClasses is going to force-read the data into R data types that I want them to be
tripClasses <- c("factor", "Date", "integer", "integer", "integer", "factor", "factor", "factor", "factor", "integer", "numeric", "numeric", "integer", "numeric", "integer", "integer", "numeric","integer","numeric")
trips <- read.csv("data/trips_per_day_09_19.csv", colClasses = tripClasses)
rm(tripClasses) #cleanup environment

#####################
### ADD VARIABLES ###
#####################

### NORMALIZE START DATA BY STATION - CALC Z SCORE
# Calc the Z score of the trips per day, by monthly average for that station
# The purpose of this (I think) is to know, for a given station, how many trips is that relative to the std dev of that station
# Because it is hard to compare total trips when certain stations have SOOOO many trips
trips$ztrips <- ave(trips$total.start, trips$id, FUN=scale)

### RECLASSIFY DAY OF WEEK AS WEEKDAY or WEEKEND
# Categorize weekday/weekend
trips$weekdayend <- ifelse(trips$weekday %in% c("Sunday","Saturday"), "weekend", "weekday")


### K-MEANS CLUSTER

# It seems like it might be helpful, rather than having a factor with 328 levels (each station), maybe to have a grouping
# of activity level, so we could have low-medium-high or some such thing? Would we be able to use clustering models for this?
# This would define an attribute of the station

# Critical question: Is it inappropriate to use k-means on single-vector data? Most of the examples seem to use two-dimensional

set.seed(1)
total_daily_rides_per_stn <- trips %>% group_by(id) %>% summarise(Trips = sum(total.start))
avg_daily_rides_per_stn <- trips %>% group_by(id) %>% summarise(Trips = mean(total.start))
km.out = kmeans(avg_daily_rides_per_stn$Trips, 3 , nstart = 20)
km.out
ggplot(data = avg_daily_rides_per_stn, aes(id, Trips, colour = km.out$cluster)) +
  geom_point(size = 2) +
  ggtitle("K-Means Clustering to Determine Activity Level")

temp <- data.frame(avg_daily_rides_per_stn$id,as.factor(km.out$cluster))
names(temp) <- c("id","activity_class")

levels(temp$activity_class) <- c("low", "medium", "high")

# maybe add these classes back to the trips data?
# DANGER: APPENDING TO SELF; DO NOT RUN MULTIPLE TIMES
#trips <- merge(trips, temp)
trips <- merge(trips, temp)
rm(temp) # cleanup

### ATTACH TRIPS
# Don't wanna be typing "trips$" all day :)
attach(trips)

#######################
### First, some EDA ###
#######################

summary(trips)

# What is the distribution of flow across stations?
hist(flow, breaks = 50)

# What is the distribution of total rides per station per day?
hist(total.start, breaks = 50)
hist(ztrips, breaks = 50)

# What is the distribution of flow for the month? (Ignore the id plot)
trips %>% 
  group_by(id) %>% 
  summarize(Flow = sum(flow)) %>%
  select(Flow) %>%
  boxplot()

# What is the distribution of rides per station for the month? (Ignore the id plot)
trips %>% 
  group_by(id) %>% 
  summarize(Trips = sum(total.start)) %>%
  select(Trips) %>%
  boxplot()

# Which days are busiest?
table(weekday)
plot(table(weekday))
# These should all be about the same, but Sunday and Monday have the highest counts.
# Why? There are 5 of each in this month, vs 4 of each for the other days.

# Total trips system-wide by day
## These datasets have 30 records, one for each day. Used to find patterns.
daily <- trips %>% group_by(date, weekday, weekdayend, Precipitation, TempAvg) %>% summarize(starts = sum(total.start), ends = sum(total.end))
dailyz <- trips %>% group_by(date, weekday, weekdayend, Precipitation, TempAvg) %>% summarize(avgz = mean(ztrips))

# PLOT: Daily Starts (Rides) by Date
#plot(daily$date,daily$starts)
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

# Precip on Starts
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

lm_avgtemp <- lm(daily$TempAvg,)

# What about distance to T Stops and Bike Lanes?

plot(tstopdist,flow) # Is there overlap? Like, T-Stops are designed around more people. More people = more potential riders
plot(bikelanedist,flow)


# Flow by Activity Class
# Using median
trips %>%
  #mutate(activity_class = fct_reorder(activity_class, flow, .fun='mean')) %>%
  ggplot( aes(x=activity_class, y=flow, fill=activity_class)) + 
  geom_boxplot() +
  xlab("activity class") +
  theme(legend.position="none") +
  xlab("Activity Class") +
  ylab("Flow")+
  ggtitle("Distribution of Flow by Activity Class (Number of Rentals)")

# High activity flow to low and medium activity stations
aggregate(flow ~ activity_class, trips, sum)



############################
### MODELING, PREDICTION ###
############################

set.seed(517)
# Create a vector for training, sampling 75% (or .75) of the dataset
train.v <- sample(nrow(trips), nrow(trips) * .75, replace = FALSE)

trips.train <- trips[train.v,]
trips.test <- trips[-train.v,]


lm1 <- lm(total.start ~ activity_class, data = trips)
summary(lm1) 
plot(lm1)



lm2 <- lm(flow ~ id, data = trips)

## work on ascending order based on monthly trips
asco <- avg_daily_rides_per_stn[order(avg_daily_rides_per_stn$Trips),]$id

