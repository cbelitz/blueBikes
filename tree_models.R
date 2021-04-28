# install.packages("randomForest")
# install.packages("gbm")

library(randomForest)
library(gbm)


# set factors
trips_per_day_09_19$weekday = as.factor(trips_per_day_09_19$weekday)
trips_per_day_09_19$District = as.factor(trips_per_day_09_19$District)
trips_per_day_09_19$Public = as.factor(trips_per_day_09_19$Public)

# remove missing values
clean_trips = na.omit(trips_per_day_09_19)

# get training set
set.seed(1)
train = sample(1:nrow(clean_trips), nrow(clean_trips)/4)
trips.test=clean_trips[-train ,"flow"]

# try one try to start
set.seed(1)
rf.trips = randomForest(flow~weekday+Name+District+Public+Total.docks+bikelanedist+tstopdist,
                        data=clean_trips,
                        subset=train,
                        mtry=6,
                        ntree=25,
                        na.action=na.exclude,
                        importance=TRUE)
yhat.rf = predict(rf.trips, newdata=clean_trips[-train,])

mean((yhat.rf-trips.test)^2) #test MSE is 75, so on average off by ~8 
importance(rf.trips)
varImpPlot(rf.trips)  # most important is number of docks, district, and then bike lane dist

# do cross-validation for best mtry and ntree
# set m values for p, p/2 and sqRoot of p
# subtract one from number of columns to account for outcome variable
m.values = c(7, floor(7/2), floor(7^(1/2)))
all_err = list()
j = 1

for (m in m.values) {
  # create an empty vector for the errors
  errors = vector(mode="list")
  for (i in seq(10, 100, by=10)) {
    set.seed(1)
    rf.trips = randomForest(flow~weekday+Name+District+Public+Total.docks+bikelanedist+tstopdist,
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

length(all_err[[1]])
x = seq(1, 10)
x = x*10
plot(x, all_err[[1]], ylim=c(63,84), xlab="Number of Trees", ylab="Test Set MSE")
points(x, all_err[[2]], col="red")
points(x, all_err[[3]], col="green")
lines(x, all_err[[1]])
lines(x, all_err[[2]], col="red")
lines(x, all_err[[3]], col="green")
legend(x="topright", legend=c("m=p", "m=p/2", expression(m==sqrt(p))), fill=c("black", "red", "green"))

# m=3 and m=2 perform similarly, which makes sense. M=2 is slightly better, so we will use that. It looks like it stabilizes around 30 trees


# try bagging
clean_trips$Name = as.factor(clean_trips$Name)
set.seed(1)
boost.trips = gbm(flow~weekday+Name+District+Total.docks+bikelanedist+tstopdist,
                  data=clean_trips,
                  distribution = "bernoulli",
                  n.trees = 1000,
                  interaction.depth = 3) 
summary(boost.trips) #most important are name, weekday

#predict on test set for error
yhat.boost= predict(boost.trips, newdata=clean_trips[-train,], n.trees=1000, type="response")
mean((yhat.boost-trips.test)^2) #test MSE is 81, so on average off by 9

