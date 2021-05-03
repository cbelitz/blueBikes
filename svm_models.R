library(e1071)

### Predict Flow ###

# remove missing values
clean_trips = na.omit(trips)

# get training set (approx 1 week of data)
set.seed(1)
train = sample(1:nrow(clean_trips), nrow(clean_trips)/4)
trips.test=clean_trips[-train ,"flow"]

# try support vector regression?
set.seed(1)
svr.trips = svm(flow~weekdayend+Name+District+Total.docks+bikelanedist+tstopdist+total.start+TempAvg+WindAvg+Precipitation,
                data=clean_trips,
                subset=train,
                kernel="radial", cost=0.1)
summary(svr.trips)

set.seed(1)
svr.predict = predict(svr.trips, newdata=clean_trips[-train,])
mean((svr.predict-trips.test)^2) #test MSE is 104
plot(svr.predict-trips.test) 
plot(trips.test, svr.predict)

# repeat with polynomial kernel
set.seed(1)
svr.trips.poly = svm(flow~weekdayend+Name+District+Total.docks+bikelanedist+tstopdist+total.start+TempAvg+WindAvg+Precipitation,
                data=clean_trips,
                subset=train,
                kernel="polynomial", cost=1)
summary(svr.trips.poly)

set.seed(1)
svr.predict.poly = predict(svr.trips.poly, newdata=clean_trips[-train,])
mean((svr.predict.poly-trips.test)^2) # test MSE is 104
plot(svr.predict.poly-trips.test) 
plot(trips.test, svr.predict.poly)

# do some tuning on polynomial kernel 
set.seed(1)
tune.trips = tune(svm, flow~weekday+Name+District+Total.docks+bikelanedist+tstopdist,
                  data=clean_trips[train,],
                  kernel="radial",
                  ranges=list(cost=c(0.1, 1, 5, 10),
                              gamma=c(0.001, 0.01, 0.1, 0.5)))


tune.trips$best.parameters # cost of 10 and gamma of 0.1
tune.trips$best.performance #MSE 65
set.seed(1)
svr.predict = predict(tune.trips$best.model, newdata=clean_trips[-train,])
mean((svr.predict-trips.test)^2) #134


# do some tuning on radial kernel 
set.seed(1)
tune.trips = tune(svm, flow~weekday+Name+District+Total.docks+bikelanedist+tstopdist,
                 data=clean_trips[train,],
                 kernel="radial",
                 ranges=list(cost=c(0.1, 1, 5, 10),
                             gamma=c(0.001, 0.01, 0.1, 0.5)))


tune.trips$best.parameters # cost of 10 and gamma of 0.1
tune.trips$best.performance #MSE 65
set.seed(1)
svr.predict = predict(tune.trips$best.model, newdata=clean_trips[-train,])
mean((svr.predict-trips.test)^2) #134
plot(trips.test, svr.predict)

#best is at highest values, so we can run again with a few higher and see if they continue to improve
set.seed(1)
tune.trips2 = tune(svm, flow~weekday+Name+District+Total.docks+bikelanedist+tstopdist,
                  data=clean_trips[train,],
                  kernel="radial",
                  ranges=list(cost=c(5, 10, 15, 20, 30),
                              gamma=c(0.1, 0.5, 1, 2,3,4)))

tune.trips2$best.parameters
tune.trips2$best.performance 
# best gamma is still 0.1 and best cost is 10

tune.trips2$performances

set.seed(1)
svr.predict = predict(tune.trips2$best.model, newdata=clean_trips[-train,])
mean((svr.predict-trips.test)^2) #still 134
plot(svr.predict-trips.test) 
plot(trips.test, svr.predict)


### Predict start.trips ### 

#support vector regression
starts.test = clean_trips[-train ,"total.start"]
set.seed(1)
svr.starts = svm(total.start~weekdayend+Name+District+Total.docks+bikelanedist+tstopdist+TempAvg+WindAvg+Precipitation,
                data=clean_trips,
                subset=train,
                kernel="radial", cost=0.1)
summary(svr.starts)

svr.starts.predict = predict(svr.starts, newdata=clean_trips[-train,])
mean((svr.starts.predict-starts.test)^2) #test MSE is 1892, so on average off by ~43.5 trips --> seems like it is skewed to high numbers
plot(svr.starts.predict-starts.test) #definitely not centered around 0
plot(starts.test, svr.starts.predict) #not handling large numbers well at all

#tune
#best is at highest values, so we can run again with a few higher and see if they continue to improve
set.seed(1)
tune.starts2 = tune(svm, total.start~weekdayend+Name+District+Total.docks+bikelanedist+tstopdist+TempAvg+WindAvg+Precipitation,
                   data=clean_trips[train,],
                   kernel="radial",
                   ranges=list(cost=c(5, 10, 15, 20, 30),
                               gamma=c(0.1, 0.5, 1, 2,3,4)))

tune.starts2$best.parameters
tune.starts2$best.performance #much better! 377 MSE so off by ~20 trips
# best gamma is .1 and best cost is 20
tune.predict = predict(tune.starts2$best.model, newdata=clean_trips[-train,])
mean((tune.predict-starts.test)^2) #test MSE is 340, so on average off by ~18 
plot(tune.predict-starts.test) #much more centered around 0
plot(starts.test, tune.predict, xlab="Starts (Data)", ylab="Starts (Predicted)", main="Radial SVM") #actually looks  decent. definitely a lot going on at the bottom there
tune.starts2$best.model
