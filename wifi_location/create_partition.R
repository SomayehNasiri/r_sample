#########Preparing data for modeling 
#########Creating partition


if (!require(pacman)) install.packages("pacman")
pacman::p_load( stats,caret,readr,ggplot2,mlbench,doParallel,PerformanceAnalytics,dplyr,
                kknn,ModelMetrics,randomForest,e1071,foreach,MLmetrics,FactoMineR)




registerDoParallel(cores = 3)#to speed up process
R_MAX_MEM_SIZE <- memory.limit(size = 40000)#Increase memory limit


#Longitude - create data partition####
set.seed(601)
trainIndex5 <- createDataPartition(y = trainingData$LONGITUDE,
                                   p = .70,
                                   list = FALSE)
LOTrain <- as.data.frame(trainingData[trainIndex5,])
LOTest <-  as.data.frame(trainingData[-trainIndex5,])

#Latitude - create data partition####
set.seed(601)
trainIndex6 <- createDataPartition(y = trainingData$LATITUDE,
                                   p = .70,
                                   list = FALSE)
LATrain <- as.data.frame(LA[trainIndex6,])
LATest <-  as.data.frame(LA[-trainIndex6,])

#Altitude(Floor) - create data partition####
set.seed(601)
trainIndex7 <- createDataPartition(y = trainingData$FLOOR,
                                   p = .70,
                                   list = FALSE)
FLTrain <- as.data.frame(FL[trainIndex7,])
FLTest <-  as.data.frame(FL[-trainIndex7,])


