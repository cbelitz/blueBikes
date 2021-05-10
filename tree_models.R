# install.packages("randomForest")
# install.packages("gbm")

library(randomForest)
library(gbm)

cross_validate_flow = function(m.values, i.values, dataset, data.subset, data.test) {
  all_err = list()
  j = 1
  for (m in m.values) {
    # create an empty vector for the errors
    errors = vector(mode="list")
    for (i in i.values) {
      set.seed(1)
      rf.trips = randomForest(flow~weekdayend+District+Total.docks+bikelanedist+tstopdist+total.start+TempAvg+WindAvg+Precipitation,
                              data=dataset,
                              subset=data.subset,
                              mtry=m,
                              ntree=i)
      yhat.rf = predict(rf.trips, newdata=dataset[-data.subset,])
      errors = c(errors, mean((yhat.rf-data.test)^2))
    }
    all_err[j] = list(errors)
    j = j + 1
  }
  return (all_err)
}

  
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

mean((yhat.rf-trips.test)^2) #test MSE is 143
importance(rf.trips)
varImpPlot(rf.trips)  # most important is start, t/bikelanedist, total.docks

# do cross-validation for best mtry and ntree
# set m values for p, p/2 and sqRoot of p
# subtract one from number of columns to account for outcome variable
all_err1 = cross_validate_flow(c(9, floor(9/2), floor(9^(1/2))),
                               seq(10, 100, by=10),
                               clean_trips,
                               train,
                               trips.test
)

x = seq(1, 10)
x = x*10
plot(x, all_err1[[1]], ylim=c(130,150), xlab="Number of Trees", ylab="Test Set MSE")
points(x, all_err1[[2]], col="red")
points(x, all_err1[[3]], col="green")
lines(x, all_err1[[1]])
lines(x, all_err1[[2]], col="red")
lines(x, all_err1[[3]], col="green")
legend(x="topright", legend=c("m=p", "m=p/2", expression(m==sqrt(p))), fill=c("black", "red", "green"))



# m=p=9 performs similarly to m=3 so we will use that since it is faster. It looks like it improves all the way up to 100 trees
set.seed(1)
rf.trips.2 = randomForest(flow~weekdayend+District+Total.docks+bikelanedist+tstopdist+total.start+TempAvg+WindAvg+Precipitation,
                        data=clean_trips,
                        subset=train,
                        mtry=3,
                        ntree=100,
                        importance=TRUE)
set.seed(1)
yhat.rf.2 = predict(rf.trips.2, newdata=clean_trips[-train,])
mean((yhat.rf.2-trips.test)^2) #test MSE is 133, so on average off by ~11.5 (slightly better than initial model)
plot(yhat.rf.2-trips.test) #no pattern to the residual, which is good
rf.trips.2 #40% of variance explained, essentially the same
importance(rf.trips.2) # most important are now total.start, total.docks, bitkelane/tstop
varImpPlot(rf.trips.2)


# try boosting
set.seed(1)
boost.trips = gbm(flow~weekdayend+District+Total.docks+bikelanedist+tstopdist+total.start+TempAvg+WindAvg+Precipitation,
                  data=clean_trips,
                  distribution = "gaussian",
                  n.trees = 1000,
                  interaction.depth = 6) 
summary(boost.trips) #most important are total.start,total.docks, bikelane/tstopdist
# plot(boost.trips)

#predict on test set for error
set.seed(1)
yhat.boost= predict(boost.trips, newdata=clean_trips[-train,], n.trees=1000)
mean((yhat.boost-trips.test)^2) #test MSE is 146, so on average off by 12
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

mean((yhat.rf-starts.test)^2) #test MSE is 151, so on average off by 12 (better than SVM)
importance(rf.starts)
varImpPlot(rf.starts)  # most important is tstopdist, bikelanedist, weekdayend
# wind goes up, rides go down, but very small effect


# do cross-validation for best mtry and ntree
# set m values for p, p/2 and sqRoot of p
m.values = c(8, floor(8/2), floor(8^(1/2)))
all_err_starts = list()
j = 1

for (m in m.values) {
  # create an empty vector for the errors
  errors = vector(mode="list")
  for (i in seq(10, 100, by=10)) {
    set.seed(1)
    rf.starts = randomForest(total.start~weekdayend+District+Total.docks+bikelanedist+tstopdist+TempAvg+WindAvg+Precipitation,
                            data=clean_trips,
                            subset=train,
                            mtry=m,
                            ntree=i)
    yhat.rf = predict(rf.starts, newdata=clean_trips[-train,])
    errors = c(errors, mean((yhat.rf-starts.test)^2))
  }
  all_err_starts[j] = list(errors)
  j = j + 1
}

x = seq(1, 10)
x = x*10
plot(x, all_err_starts[[1]], ylim=c(140,170), xlab="Number of Trees", ylab="Test Set MSE", main="CV for RF for Starts")
points(x, all_err_starts[[2]], col="red")
points(x, all_err_starts[[3]], col="green")
lines(x, all_err_starts[[1]])
lines(x, all_err_starts[[2]], col="red")
lines(x, all_err_starts[[3]], col="green")
legend(x="topright", legend=c("m=p", "m=p/2", expression(m==sqrt(p))), fill=c("black", "red", "green"))

# use best model for confirmation
set.seed(1)
rf.starts.best = randomForest(total.start~weekdayend+District+Total.docks+bikelanedist+tstopdist+TempAvg+WindAvg+Precipitation,
                         data=clean_trips,
                         subset=train,
                         mtry=8,
                         ntree=60)
yhat.rf = predict(rf.starts.best, newdata=clean_trips[-train,])
mean((yhat.rf-starts.test)^2) #test MSE is 147, so on average off by 12 (better than SVM)
importance(rf.starts.best)
varImpPlot(rf.starts.best)
summary(rf.starts.best$rsq)


# try boosting
set.seed(1)
boost.starts = gbm(total.start~weekdayend+District+Total.docks+bikelanedist+tstopdist+TempAvg+WindAvg+Precipitation,
                  data=clean_trips,
                  distribution = "gaussian",
                  n.trees = 1000,
                  interaction.depth = 6) 
summary(boost.starts) #most important are bikelane/tstopdist,total.docks
plot(boost.starts)
#predict on test set for error
set.seed(1)
yhat.boost= predict(boost.starts, newdata=clean_trips[-train,], n.trees=1000)
mean((yhat.boost-starts.test)^2) #test MSE is 3575, so on average off by ~59 
plot(yhat.boost-starts.test) #maybe not centered around 0 (error tends to be positive/some outliers)
###this should be explored. perhaps we need to do the model with centered trips

# try boosting on centered trips
starts.centered.test = clean_trips[-train ,"ztrips"]
set.seed(1)
boost.starts.centered = gbm(ztrips~weekdayend+District+Total.docks+bikelanedist+tstopdist+TempAvg+WindAvg+Precipitation,
                   data=clean_trips,
                   distribution = "gaussian",
                   n.trees = 1000,
                   interaction.depth = 6) 
summary(boost.starts.centered) #most important are bikelane/tstopdist, wind and temp avg

#predict on test set for error
set.seed(1)
yhat.boost= predict(boost.starts.centered, newdata=clean_trips[-train,], n.trees=1000)
mean((yhat.boost-starts.centered.test)^2) #test MSE is .55, so on average off by 0.74 standard dev, which is 32 trips. Definitely better
plot(yhat.boost-starts.centered.test) #now centered around 0
# overall this is slightly better than linear model without names


########################
#### RF by Activity ####
########################


### let's see if we can predict starts on "high" usage stations only ###
high = subset(clean_trips, clean_trips$activity_class=="high")
medium = subset(clean_trips, clean_trips$activity_class=="medium")
low = subset(clean_trips, clean_trips$activity_class=="low")

# check that this worked as expected
nrow(clean_trips)
nrow(high) + nrow(medium) + nrow(low)

### Random forests for high alone
set.seed(1)
train_high = sample(1:nrow(high), nrow(high)*0.75)
trips.test.high=high[-train_high ,"flow"]

set.seed(1)
flow.high = randomForest(flow~weekdayend+District+Total.docks+bikelanedist+tstopdist+total.start+TempAvg+WindAvg+Precipitation,
                        data=high,
                        subset=train_high,
                        mtry=6,
                        ntree=25)


#predict on test set for error
set.seed(1)
yhat.boost=predict(flow.high, newdata=high[-train_high,])
mean((yhat.boost-trips.test.high)^2) #test MSE is 841 --> not ideal but could be worse
plot(yhat.boost-trips.test.high) #not super centered around 0

# cross-validation
all_err.high = cross_validate_flow(c(9, floor(9/2), floor(9^(1/2))),
                                   seq(10, 100, by=10),
                                   high,
                                   train_high,
                                   trips.test.high
)

x = seq(1, 10)
x = x*10
plot(x, all_err.high[[1]], xlab="Number of Trees", ylab="Test Set MSE", ylim=c(700, 1000), main="CV for RF for Flow")
points(x, all_err.high[[2]], col="red")
points(x, all_err.high[[3]], col="green")
lines(x, all_err.high[[1]])
lines(x, all_err.high[[2]], col="red")
lines(x, all_err.high[[3]], col="green")
legend(x="topright", legend=c("m=p", "m=p/2", expression(m==sqrt(p))), fill=c("black", "red", "green"))

# best model
set.seed(1)
flow.high = randomForest(flow~weekdayend+District+Total.docks+bikelanedist+tstopdist+total.start+TempAvg+WindAvg+Precipitation,
                         data=high,
                         subset=train_high,
                         mtry=floor(9/2),
                         ntree=80)
#predict on test set for error
set.seed(1)
yhat.high=predict(flow.high, newdata=high[-train_high,])
mean((yhat.high-trips.test.high)^2) #test MSE is 749 --> better than first model
plot(yhat.high-trips.test.high) #nore centered around 0 with some outliers


## Medium activity class ##

# one to start
set.seed(1)
train_medium = sample(1:nrow(medium), nrow(medium)*0.75)
trips.test.medium=medium[-train_medium ,"flow"]

set.seed(1)
flow.medium = randomForest(flow~weekdayend+District+Total.docks+bikelanedist+tstopdist+total.start+TempAvg+WindAvg+Precipitation,
                         data=medium,
                         subset=train_medium,
                         mtry=6,
                         ntree=25)

#predict on test set for error
set.seed(1)
yhat.medium=predict(flow.medium, newdata=medium[-train_medium,])
mean((yhat.medium-trips.test.medium)^2) #test MSE is 57 --> much lower!
plot(yhat.medium-trips.test.medium) #centered around 0

# cross-validation
all_err.medium = cross_validate_flow(c(9, floor(9/2), floor(9^(1/2))),
                                   seq(10, 100, by=10),
                                   medium,
                                   train_medium,
                                   trips.test.medium
)

plot(x, all_err.medium[[1]], xlab="Number of Trees", ylab="Test Set MSE", ylim=c(55, 65), main="CV for RF for Flow")
points(x, all_err.medium[[2]], col="red")
points(x, all_err.medium[[3]], col="green")
lines(x, all_err.medium[[1]])
lines(x, all_err.medium[[2]], col="red")
lines(x, all_err.medium[[3]], col="green")
legend(x="topright", legend=c("m=p", "m=p/2", expression(m==sqrt(p))), fill=c("black", "red", "green"))
# all values of m quite similar, but generally seems like 3 or 4 is best at high numbers of trees

# best model
set.seed(1)
flow.medium.best = randomForest(flow~weekdayend+District+Total.docks+bikelanedist+tstopdist+total.start+TempAvg+WindAvg+Precipitation,
                                data=medium,
                                subset=train_medium,
                                mtry=4,
                                ntree=100)
#predict on test set for error
set.seed(1)
yhat.medium=predict(flow.medium, newdata=medium[-train_medium,])
mean((yhat.medium-trips.test.medium)^2) #test MSE is 57 --> same as first model lol
plot(yhat.medium-trips.test.medium) #centered around 0 still


## low activity class ##

# one to start
set.seed(1)
train_low = sample(1:nrow(low), nrow(low)*0.75)
trips.test.low=low[-train_low ,"flow"]

set.seed(1)
flow.low = randomForest(flow~weekdayend+District+Total.docks+bikelanedist+tstopdist+total.start+TempAvg+WindAvg+Precipitation,
                           data=low,
                           subset=train_low,
                           mtry=6,
                           ntree=25)

#predict on test set for error
set.seed(1)
yhat.low=predict(flow.low, newdata=low[-train_low,])
mean((yhat.low-trips.test.low)^2) #test MSE is 20 --> much smaller
plot(yhat.low-trips.test.low) #centered around 0

# cross-validation
all_err.low = cross_validate_flow(c(9, floor(9/2), floor(9^(1/2))),
                                     seq(10, 100, by=10),
                                     low,
                                     train_low,
                                     trips.test.low
)

plot(x, all_err.low[[1]], xlab="Number of Trees", ylab="Test Set MSE", ylim=c(19, 23), main="CV for RF for Flow")
points(x, all_err.low[[2]], col="red")
points(x, all_err.low[[3]], col="green")
lines(x, all_err.low[[1]])
lines(x, all_err.low[[2]], col="red")
lines(x, all_err.low[[3]], col="green")
legend(x="topright", legend=c("m=p", "m=p/2", expression(m==sqrt(p))), fill=c("black", "red", "green"))
# all values of m quite similar, but generally seems like 3 is best and stabilizes around 50

# best model
set.seed(1)
flow.low.best = randomForest(flow~weekdayend+District+Total.docks+bikelanedist+tstopdist+total.start+TempAvg+WindAvg+Precipitation,
                        data=low,
                        subset=train_low,
                        mtry=3,
                        ntree=60)

#predict on test set for error
set.seed(1)
yhat.low=predict(flow.low, newdata=low[-train_low,])
mean((yhat.low-trips.test.low)^2) #test MSE is 20 --> same as first model
plot(yhat.low-trips.test.low) #centered around 0