library(mlbench)
library(caret)
library(ggplot2)
library(lattice)
library(randomForest)

responseDs <- read.csv(file="/home/mohsen/R/data/CompleteResponses.csv",header=TRUE,sep = ",")
predictionDs<- read.csv(file="/home/mohsen/R/data/SurveyIncomplete.csv",header=TRUE,sep = ",")


set.seed(567)

##### calculate correlation matrix
correlationMatrix <- cor(responseDs[,1:7])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

#### Conver attributes to factor
responseDs$brand <- as.factor(responseDs$brand)
responseDs$zipcode<-as.factor(responseDs$zipcode)
responseDs$car<-as.factor(responseDs$car)
responseDs$elevel<-as.ordered(responseDs$elevel)

#######Exclude some columns
testDs<- responseDs  ## subset(responseDs, select = -c(zipcode,car,elevel) )

set.seed(107)
responseTrain <- createDataPartition(y = testDs$brand,
                                     p = .75,
                                     list = FALSE)

## The output is a set of integers, for the rows of Response DS
rTraining <- testDs[responseTrain,]
rTesting <- testDs[-responseTrain,]
nrow(rTraining)
nrow(rTesting)
#####cross validation

metric <- "Accuracy"
mtry <- expand.grid(mtry=c(4))
tunegrid <- expand.grid(.mtry=mtry)


rfitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1, search = 'random')
#train Random Forest Regression model
rfFitr2 <- train(brand~.,
                 data = rTraining,
                 method = "rf",
                 metric = metric,
                 ntree=100,
                 tuneGrid = tunegrid,
                 trControl = rfitControl)
print(rfFitr2)

preTrain <- predict(rfFitr2,rTraining)
confusionMatrix(preTrain,rTraining$brand)

preTest <- predict(rfFitr2,rTesting)
confusionMatrix(preTest,rTesting$brand)

preBrand <- predict(rfFitr2,predictionDs)
predictionDs$brand <- preBrand

#########
# estimate variable importance
importance <- varImp(rfFitr2, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

####RE sample
res_mentor <- postResample(preTest, rTesting$brand)
res_mentor


###Create table from data
tab <- table(preTest,rTesting$brand)
tab


ggplot(preTest~rTesting$brand,type = "p",
     main="Predicted Values vs Real values",
      lty=1.8, col="red")
ggplot(rTesting,aes(brand~salary))
ggplot(newAsData,aes(Plant~Petal.Length))
sp(preTest, rTesting$brand)

#############try modeling for LM
require(ggplot2)
DiabetesData$Predicted<-glm2$fitted.values
ggplot(rTesting,aes(x=salary,y=credit,color=preTest))+
  geom_point(size=2,alpha=0.5)

ggplot(DiabetesData,aes(x=BMI,y=Glucose,color=Outcome==(Predicted>0.5)))+
  geom_point(size=2,alpha=0.5)




###########running model without cross validation
rf <- randomForest(brand~.,data=rTraining)
ctrl
rf <- randomForest(brand~.,data=rTraining,
                   ntree=100,
                   mtry=4,
                   importance=TRUE,
                   proximity=TRUE)
print(rf)
p1 <- predict(rf,rTraining)

head(p1)
head(rTraining)
confusionMatrix(p1,rTraining$brand)

hist(treesize(rf),
     main="No. of nodes for the Trees",
     col="lightgray")
## Importance of variables in model

varImpPlot(rf,
           sort=T,
           n.var=5,
           main="Top-variables Importance")
## mix of these two commands we know which variables how many times used in model
## Displays attributes according to their contribution in model
importance(rf)
#Number of times var used while modeling
varUsed(rf)

p2 <- predict(rf,rTesting)
confusionMatrix(p2,rTesting$brand)
plot(rf)
##To find numbaer of trees suitable for model
t1 <- tuneRF(rTraining[,-7],rTraining[,7],
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 100,
       trace=TRUE,
       improve = 0.05)

MDSplot(rf,rTraining$brand)
