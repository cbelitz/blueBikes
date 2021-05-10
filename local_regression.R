library(gam)

ctrips.predictors.start

ctrips.predictors.flow


localfit.starts = loess(total.start~bikelanedist, span=0.2, data = ctrips.predictors.flow[train.v,])
localfit.starts2 = loess(total.start~bikelanedist, span=0.5, data = ctrips.predictors.flow[train.v,])
localfit.starts3 = loess(total.start~bikelanedist, span=0.7, data = ctrips.predictors.flow[train.v,])
anova(localfit.starts, localfit.starts2) #second is better
anova(localfit.starts2, localfit.starts3) # 0.7 best

poly.starts = lm(total.start~poly(bikelanedist,3), data = ctrips.predictors.flow[train.v,])
summary(poly.starts)

plot(localfit.starts3) #not great. probably need a gam

gam.starts = gam(total.start~.-c(date) + poly(bikelanedist, 3),
                 data = ctrips.predictors.flow[train.v,])
summary(gam.starts)
plot(gam.starts)


poly.trips = lm(flow~poly(total.start,3), data = ctrips.predictors.flow[train.v,])
summary(poly.trips)

set.seed(1)
gam.flow = gam(flow~.-total.start + s(total.start, 5),
                data = ctrips.predictors.flow[train.v,])
summary(gam.flow)
plot(gam.flow)
predict.flow = predict(gam.flow, newdata=ctrips.predictors.start[-train.v,])
summary(predict.trips)
pred.flow.MSE <- mean((predict.flow - ctrips.predictors.start$total.start[-train.v])^2)
pred.flow.MSE
# no better than without spline for start