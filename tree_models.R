# install.packages("randomForest")
# install.packages("gbm")

library(randomForest)
library(gbm)

# get training set
set.seed(1)
train = sample(1:nrow(trips_per_day_09_19), nrow(trips_per_day_09_19)/4)
trips.test=trips_per_day_09_19[-train ,"flow"]

# set factors
trips_per_day_09_19$weekday = as.factor(trips_per_day_09_19$weekday)
trips_per_day_09_19$District = as.factor(trips_per_day_09_19$District)
trips_per_day_09_19$Public = as.factor(trips_per_day_09_19$Public)


# try one try to start
set.seed(1)
rf.trips = randomForest(flow~weekday+Name+District+Public+Total.docks+bikelanedist+tstopdist,
                        data=trips_per_day_09_19,
                        subset=train,
                        mtry=6,
                        ntree=25,
                        na.action=na.exclude,
                        importance=TRUE)
yhat.rf = predict(rf.trips, newdata=trips_per_day_09_19[-train,])
#need to figure out how to skip NA
mean((yhat.rf-trips.test)^2)
importance(rf.trips)
varImpPlot(rf.trips)   
