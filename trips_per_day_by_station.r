# install.packages("plyr") # for aggregation
library(plyr)

# read in trip data for Sep 2019. A bit slow but only needs to be done once
temp = tempfile()
download.file("https://s3.amazonaws.com/hubway-data/201909-bluebikes-tripdata.zip", temp)
trip_zip = unz(temp, "201909-bluebikes-tripdata.csv")
trip_data = read.csv(trip_zip, header=TRUE)
unlink(temp)
summary(trip_data)

# extract just date from start times
trip_data$date = as.Date(trip_data$starttime)

# remove outlier trips. start with 3 std dev, adjust if appropriate
outlier_threshold = mean(trip_data$tripduration) + 3*sd(trip_data$tripduration)
trip_data = subset(trip_data, trip_data[, "tripduration"] < outlier_threshold)

# create empty data frame to fill after transformations.
# numrow is fixed based on number of stations*dates, need to adjust if using a different month
agg_trips = data.frame(matrix(0, ncol=4, nrow=9529))
colnames(agg_trips) = c('id', 'date', 'total.start', 'total.end')

# count number of trips in and out of a station by day
trips.start = count(trip_data, c('date', 'start.station.id'))
trips.end = count(trip_data, c('date', 'end.station.id'))

# rename columns
colnames(trips.start)[colnames(trips.start) == "start.station.id"] = "id"
colnames(trips.end)[colnames(trips.end) == "end.station.id"] = "id"

# merge in and out counts and replace NA values with 0 (since no trips)
jointdataset = merge(trips.start, trips.end, by = c('date','id'), all=TRUE)
jointdataset[is.na(jointdataset)] = 0

# fill aggregate dataset
agg_trips$id = jointdataset$id
agg_trips$date = jointdataset$date
agg_trips$total.start = jointdataset$freq.x
agg_trips$total.end = jointdataset$freq.y

# calculate flow for each station (end - start since we want to know the change)
agg_trips$flow = agg_trips$total.end - agg_trips$total.start

# add in day of the week for averages
agg_trips$weekday = weekdays(agg_trips$date)
