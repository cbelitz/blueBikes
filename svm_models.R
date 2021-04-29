library(e1071)
library(rmse)

# set factors
trips_per_day_09_19$weekday = as.factor(trips_per_day_09_19$weekday)
trips_per_day_09_19$District = as.factor(trips_per_day_09_19$District)
trips_per_day_09_19$Public = as.factor(trips_per_day_09_19$Public)

# remove missing values
clean_trips = na.omit(trips_per_day_09_19)

# get training set (approx 1 week of data)
set.seed(1)
train = sample(1:nrow(clean_trips), nrow(clean_trips)/4)
trips.test=clean_trips[-train ,"flow"]


# try support vector regression?
set.seed(1)
svr.trips = svm(flow~weekday+Name+District+Total.docks+bikelanedist+tstopdist,
                data=clean_trips,
                subset=train,
                kernel="radial", cost=1)
summary(svr.trips)

svr.predict = predict(svr.trips, newdata=clean_trips[-train,])
mean((svr.predict-trips.test)^2) #test MSE is 79, so on average off by ~8 (worse than trees but similar overall)
plot(svr.predict-trips.test) 
plot(trips.test, svr.predict)

# repeat with polynomial kernel
set.seed(1)
svr.trips.poly = svm(flow~weekday+Name+District+Total.docks+bikelanedist+tstopdist,
                data=clean_trips,
                subset=train,
                kernel="polynomial", cost=1)
summary(svr.trips.poly)

#not currently working
svr.predict.poly = predict(svr.trips.poly, newdata=clean_trips[-train,])
mean((svr.predict.poly-trips.test)^2) 
plot(svr.predict.poly-trips.test) 
plot(trips.test, svr.predict.poly)

# do some tuning on radial kernel (also failing)
tune.trips = tune(svm, flow~weekday+Name+District+Total.docks+bikelanedist+tstopdist,
                 data=clean_trips[train,],
                 kernel="radial",
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1,5),
                             gamma=c(0.1, 0.5, 1, 2)))


