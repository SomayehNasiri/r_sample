set.seed(123)
trainSizaCar<-round(nrow(cars)*0.7)
testSizeCar<-nrow(cars)-trainSizaCar

training_indices<-sample(seq_len(nrow(cars)),size=trainSizaCar)
trainSetCar<-cars[training_indices,]
testSetCar<-cars[-training_indices,]
lmCar<-lm(dist~ speed, trainSetCar)
pred<-predict(lmCar,testSetCar)
summary(lmCar)
lmCar
testSetCar
tbl_testSet<-data.frame(testSetCar$speed,testSetCar$dist)
tbl_pre<-pred
tbl_pre
ff<- subset(testSetCar)


write_csv(x = tbl)

# pred vs real values
plot(testSetCar$dist,type = "p",
     main="Predicted Values vs Real values",
     
     lty=1.8, col="red")
lines(pred,type="l",col="blue")
plot(pred,type = "l",lty=1.8, col="lightblue")

summary(lmCar)
confint(lmCar)
plot(cars)
?cars
summary(cars)

x<- c(sapply(cars, min, na.rm=TRUE),sapply(cars, mean, na.rm=TRUE) ,sapply(cars, max, na.rm=TRUE) )


barplot(x)

barplot(x,
        col="lightblue",
        main="Max,Min,Avg Speed and Distance of Cars",
        xlab = "Aggregation",
        ylab = "value")


plot(cars,
     pch=16,
     col="gray",
     main="Speed vs ditanse",
     xlab="Speed",
     ylab="Stopping Distance"
     )
abline(lm(cars$dist~cars$speed),
       col="darkred",
       lwd=2)
lines(lowess(cars$speed,cars$dist),
      col="blue",
      lwd=2)
