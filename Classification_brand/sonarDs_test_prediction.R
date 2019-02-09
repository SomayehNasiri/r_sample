
library(caret)
library(mlbench)
library(ggplot2)
data("Sonar")
responseDs
responseDs <- read.csv(file="/home/mohsen/R/data/CompleteResponses.csv",header=TRUE,sep = ",")

dd <- read.csv("Data/CompleteResponses.csv")

str(responseDs)
summary(responseDs)
##Split data
set.seed(107)
responseTrain <- createDataPartition(y = Sonar$Class,
                               p = .75,
                               list = FALSE)


## The output is a set of integers for the rows of Response DS
str(responseTrain)
rTraining <- responseDs [responseTrain,]
rTesting <- responseDs [-responseTrain,]
nrow(rTraining)
nrow(rTesting)
### Sonar
rTraining <- Sonar [responseTrain,]
rTesting <- Sonar [-responseTrain,]
nrow(rTraining)
nrow(rTesting)


##PLSA Model Tuning
ctrl <- trainControl(method = "repeatedcv",
                      repeats = 3,
                      classProbs = TRUE,
                     summaryFunction = twoClassSummary)
ctrl
plsFit <- train(Class ~.,
                data=rTraining,
                method="pls",
                tuneLength = 15,
                trControl = ctrl,
                metric = "ROC",
                preProc=c("center","scale"))
plot(plsFit)

plsClasses <- predict(plsFit, newdata = rTesting)
str(plsClasses)

plsProbs <- predict(plsFit, newdata = rTesting, type = "prob")
head(plsProbs) 
 
confusionMatrix(data = plsClasses, rTesting$Class)
rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4) 

set.seed(123)
rdaFit <- train(Class ~ .,
                   data = rTraining,
                   method = "rda",
                   tuneGrid = rdaGrid,
                   trControl = ctrl,
                   metric = "ROC")
rdaFit 

rdaClasses <- predict(rdaFit, newdata = rTesting)
confusionMatrix(rdaClasses,rTesting$Class)

resamps <- resamples(list(pls=plsFit,rda=rdaFit))
summary(resamps)

xyplot(resamps, what = "BlandAltman")

diffs <- diff(resamps)
summary(diffs)


######Feature selection
responseDs <- read.csv(file="/home/mohsen/R/data/CompleteResponses.csv",header=TRUE,sep = ",")
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(responseDs[,1:6], responseDs[,7], sizes=c(1:6), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
predictors(results)
# plot the results
results
plot(results, type=c("g", "o"))
