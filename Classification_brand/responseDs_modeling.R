install.packages("C50")
library(C50)
library(kernlab)

library(mlbench)
library(randomForest)
library(caretEnsemble)
library(caret)
library(MASS)
library(klaR)
library(nnet)

responseDs <- read.csv(file="/home/mohsen/R/data/CompleteResponses.csv",header=TRUE,sep = ",")

responseDs$brand <- as.factor(responseDs$brand)
responseDs$zipcode<-as.factor(responseDs$zipcode)

responseDs$car<-as.factor(responseDs$car)

responseDs$elevel<- as.ordered(responseDs$elevel)

set.seed(107)
responseTrain <- createDataPartition(y = responseDs$brand,
                                     p = .75,
                                     list = FALSE)


## The output is a set of integers for the rows of Response DS

rTraining <- responseDs [responseTrain,]
rTesting <- responseDs [-responseTrain,]
nrow(rTraining)
nrow(rTesting)

##Caret Model Tuning with C5 Algorithm
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, 
                     repeats=1,
                     search="random")


DecTreeModel <- train(brand ~ ., data = rTraining, 
                      method = "C5.0",
                      tuneLength=2,
                      metric="Accuracy",
                      preProcess=c("scale","center"),
                      trControl= ctrl,
                      na.action = na.omit)

DecTreeModel

DTPredTrain <-predict(DecTreeModel, rTraining, na.action = na.pass)
# Print confusion matrix and results
confusionMatrix(DTPredTrain, rTraining$brand)


DTPredTest <-predict(DecTreeModel, rTesting, na.action = na.pass)
# Print confusion matrix and results
confusionMatrix(DTPredTest, rTesting$brand)
print(DecTreeModel)

plot(varImp(DecTreeModel))






















