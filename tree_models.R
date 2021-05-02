# install.packages("randomForest")
# install.packages("gbm")

library(randomForest)
library(gbm)

# remove missing values
clean_trips = na.omit(trips)

# get training set
set.seed(1)
train = sample(1:nrow(clean_trips), nrow(clean_trips)*0.8)
trips.test=clean_trips[-train ,"flow"]

### Predict Flow ###

# try one try to start
set.seed(1)
rf.trips = randomForest(flow~weekdayend+District+Total.docks+bikelanedist+tstopdist+total.start+TempAvg+WindAvg+Precipitation,
                        data=clean_trips,
                        subset=train,
                        mtry=6,
                        ntree=25,
                        na.action=na.exclude,
                        importance=TRUE)
rf.trips # 36% of variance explained even without name

yhat.rf = predict(rf.trips, newdata=clean_trips[-train,])

mean((yhat.rf-trips.test)^2) #test MSE is 42, so on average off by ~6.5
importance(rf.trips)
varImpPlot(rf.trips)  # most important is start, bikelanedist, total.docks

# do cross-validation for best mtry and ntree
# set m values for p, p/2 and sqRoot of p
# subtract one from number of columns to account for outcome variable
m.values = c(9, floor(9/2), floor(9^(1/2)))
all_err = list()
j = 1

for (m in m.values) {
  # create an empty vector for the errors
  errors = vector(mode="list")
  for (i in seq(10, 100, by=10)) {
    set.seed(1)
    rf.trips = randomForest(flow~weekdayend+District+Total.docks+bikelanedist+tstopdist+total.start+TempAvg+WindAvg+Precipitation,
                            data=clean_trips,
                            subset=train,
                            mtry=m,
                            ntree=i)
    yhat.rf = predict(rf.trips, newdata=clean_trips[-train,])
    errors = c(errors, mean((yhat.rf-trips.test)^2))
  }
  all_err[j] = list(errors)
  j = j + 1
}

x = seq(1, 10)
x = x*10
plot(x, all_err[[1]], ylim=c(45,60), xlab="Number of Trees", ylab="Test Set MSE")
points(x, all_err[[2]], col="red")
points(x, all_err[[3]], col="green")
lines(x, all_err[[1]])
lines(x, all_err[[2]], col="red")
lines(x, all_err[[3]], col="green")
legend(x="topright", legend=c("m=p", "m=p/2", expression(m==sqrt(p))), fill=c("black", "red", "green"))

# m=p=9 performs best so we will use that. It looks like it stabilizes around 50-60 trees
set.seed(1)
rf.trips.2 = randomForest(flow~weekdayend+District+Total.docks+bikelanedist+tstopdist+total.start+TempAvg+WindAvg+Precipitation,
                        data=clean_trips,
                        subset=train,
                        mtry=9,
                        ntree=60,
                        importance=TRUE)
yhat.rf.2 = predict(rf.trips.2, newdata=clean_trips[-train,])
mean((yhat.rf.2-trips.test)^2) #test MSE is 50, so on average off by ~7 (no better than initial model?)
plot(yhat.rf.2-trips.test) #no pattern to the residual, which is good
rf.trips.2 #35.86% of variance explained, essentially the same
importance(rf.trips.2) # most important are now Distritc, bikelanedist, and tstopdist/total.docks
varImpPlot(rf.trips.2)


# try boosting
clean_trips$weekdayend = as.factor(clean_trips$weekdayend)
set.seed(1)
boost.trips = gbm(flow~weekdayend+District+Total.docks+bikelanedist+tstopdist+total.start+TempAvg+WindAvg+Precipitation,
                  data=clean_trips,
                  distribution = "gaussian",
                  n.trees = 1000,
                  interaction.depth = 6) 
summary(boost.trips) #most important are total.start,total.docks, bikelane/tstopdist
# plot(boost.trips)

#predict on test set for error
yhat.boost= predict(boost.trips, newdata=clean_trips[-train,], n.trees=1000)
mean((yhat.boost-trips.test)^2) #test MSE is 28!, so on average off by ~5.3
plot(yhat.boost-trips.test) #no patterns

### Predict Starts ###

# try one try to start
starts.test = clean_trips[-train ,"total.start"]
set.seed(1)
rf.starts = randomForest(total.start~weekdayend+District+Total.docks+bikelanedist+tstopdist+TempAvg+WindAvg+Precipitation,
                        data=clean_trips,
                        subset=train,
                        mtry=6,
                        ntree=25,
                        na.action=na.exclude,
                        importance=TRUE)
rf.trips # 38% of variance explained even without name

yhat.rf = predict(rf.starts, newdata=clean_trips[-train,])

mean((yhat.rf-starts.test)^2) #test MSE is 155, so on average off by 12 (better than SVM)
importance(rf.starts)
varImpPlot(rf.starts)  # most important is tstopdist, bikelanedist, weekdayend
# wind goes up, rides go down, but very small effect


# do cross-validation for best mtry and ntree
# set m values for p, p/2 and sqRoot of p
# subtract one from number of columns to account for outcome variable
#currently failing
m.values = c(8, floor(8/2), floor(8^(1/2)))
all_err = list()
j = 1

for (m in m.values) {
  # create an empty vector for the errors
  errors = vector(mode="list")
  for (i in seq(10, 100, by=10)) {
    set.seed(1)
    rf.startss = randomForest(total.start~weekdayend+District+Total.docks+bikelanedist+tstopdist+TempAvg+WindAvg+Precipitation,
                            data=clean_trips,
                            subset=train,
                            mtry=m,
                            ntree=i)
    yhat.rf = predict(rf.trips, newdata=clean_trips[-train,])
    errors = c(errors, mean((yhat.rf-trips.test)^2))
  }
  all_err[j] = list(errors)
  j = j + 1
}

x = seq(1, 10)
x = x*10
plot(x, all_err[[1]], xlab="Number of Trees", ylab="Test Set MSE")
points(x, all_err[[2]], col="red")
points(x, all_err[[3]], col="green")
lines(x, all_err[[1]])
lines(x, all_err[[2]], col="red")
lines(x, all_err[[3]], col="green")
legend(x="topright", legend=c("m=p", "m=p/2", expression(m==sqrt(p))), fill=c("black", "red", "green"))

# try boosting
set.seed(1)
boost.starts = gbm(total.start~weekdayend+District+Total.docks+bikelanedist+tstopdist+TempAvg+WindAvg+Precipitation,
                  data=clean_trips,
                  distribution = "gaussian",
                  n.trees = 1000,
                  interaction.depth = 6) 
summary(boost.starts) #most important are bikelane/tstopdist,total.docks

#predict on test set for error
yhat.boost= predict(boost.starts, newdata=clean_trips[-train,], n.trees=1000)
mean((yhat.boost-starts.test)^2) #test MSE is 112, so on average off by ~10 (best yet!)
plot(yhat.boost-starts.test) #definitely not centered around 0 (error tends to be positive)
###this should be explored. perhaps we need to do the model with centered trips
