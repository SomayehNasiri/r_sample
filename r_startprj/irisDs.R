summary(IrisDataset)
sl<-IrisDataset$Sepal.Length
sw<-IrisDataset$Sepal.Width
s<-IrisDataset$Species
x<- aggregate(sl~s,Fun=mean)

trainSizaCar<-round(nrow(cars)*0.7)
testSizeCar<-nrow(cars)-trainSizaCar

set.seed(123)
trainSizeIris <- round(nrow(IrisDataset) * 0.8)
testSizeIris <- nrow(IrisDataset) - trainSizeIris
training_indices<-sample(seq_len(nrow(IrisDataset)),size =trainSizeIris)
trainSetIris<-IrisDataset[training_indices,]
testSetIris<-IrisDataset[-training_indices,] 
LinearModelIris<- lm(Petal.Length~Petal.Width,trainSetIris)
predictionIris<-predict(LinearModelIris,testSetIris)  

summary(IrisDataset)
summary(LinearModelIris)


summary(predictionIris)
predictionIris

testSetframe<-data.frame(testSetIris)
w<-data.frame(predictionIris)
plot(testSetIris$Petal.Length,type = "p",
     main="Predicted VS Real Value for Petal Length",
     lty=1.8, col="red")
lines(predictionIris,type="l",col="blue")
plot(predictionIris,type = "l",lty=1.8, col="lightblue")

#############
petalmean<- aggregate(IrisDataset$Sepal.Length~IrisDataset$Species, FUN=mean)
petalmean


petalmean.data <-t(petalmean[-1])
colnames(petalmean.data)<-petalmean[,1]
barplot(petalmean.data,
        col="lightblue",
        main="Average Petal Length for Species",
        xlab = "Species",
        ylab = "mean petal length")

plot(predictionIris)
plot(IrisDataset,
     pch=16,
     col="gray",
     main="Lengh vs Width",
     xlab="width",
     ylab="length"
)
abline(lm(IrisDataset$Petal.Length~IrisDataset$Petal.Width),
       col="darkred",
       lwd=2)


plot(IrisDataset$Petal.Length, IrisDataset$Petal.Width, pch=21, bg=c("red","green3","blue")[unclass(IrisDataset$Species)], main="Iris Data", xlab="Petal length", ylab="Petal width")
abline(lsfit(IrisDataset$Petal.Length, IrisDataset$Petal.Width)$coefficients, col="darkred")

pl<-IrisDataset$Petal.Length
# plot1 Histogram
hist(pl,
     prob=TRUE,
     breaks =12,
     col="#E5E5E5",
     border=0,
     main="Petal Length from Three Species of Iris")
lines(density(pl),col="darkred",lwd=2)
rug(pl,col="darkgray",lwd = 2)
#colors
barplot(predictionIris,col="slategray3")
barplot(predictionIris,col=colors()[102])
barplot(predictionIris,col=rgb(.54,.0,.0))
barplot(predictionIris,col=c("slategray3","red",("green")))
barplot(predictionIris,col=rainbow(6))
barplot(predictionIris,col=cm.colors(6))
barplot(predictionIris,col=topo.colors(6))

