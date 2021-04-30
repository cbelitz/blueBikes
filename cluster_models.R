library(mltools)
library(data.table)

# set factors
trips_per_day_09_19$weekday = as.factor(trips_per_day_09_19$weekday)
trips_per_day_09_19$District = as.factor(trips_per_day_09_19$District)
trips_per_day_09_19$Public = as.factor(trips_per_day_09_19$Public)

# remove missing values
clean_trips = na.omit(trips_per_day_09_19)
test = one_hot(as.data.table(no_names))

test = subset(test, select=-c(1,2,3))

# k-means
set.seed(1)
kcluster.trips = kmeans(test, 20, nstart=20)
kcluster.trips
plot(test, col=(kcluster.trips$cluster+1))

pr = prcomp(test)
pr
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
