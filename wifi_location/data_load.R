
if (!require(pacman)) install.packages("pacman")
pacman::p_load( stats,caret, tidyverse,RColorBrewer,dplyr,ggplot2,plotly,
                scatterplot3d,Amelia,corrplot,mlbench,PerformanceAnalytics
                ,psych,readr,GGally,doParallel,kknn,ModelMetrics,
                randomForest,e1071,foreach,MLmetrics,FactoMineR,pROC,anytime)





trainingData <- read.csv(file = "data/trainingData.csv")
validationData <- read.csv(file = "data/validationData.csv")

str(wifi_training_ds)
write.csv(summary(wifi_training_ds), "data/new_training2.csv",
          row.names = FALSE)

 
 Stats1 <- data.frame(
   Min = apply(trainingData, 2, min), # minimum
   Q1 = apply(trainingData, 2, quantile, 1/4), # First quartile
   Med = apply(trainingData, 2, median), # median
   Mean = apply(trainingData, 2, mean), # mean
   Q3 = apply(trainingData, 2, quantile, 3/4), # Third quartile
   Max = apply(trainingData, 2, max) # Maximum
 )
 head(Stats1,5)
 
 Stats2 <- data.frame(
   Min = apply(validationData, 2, min), # minimum
   Q1 = apply(validationData, 2, quantile, 1/4), # First quartile
   Med = apply(validationData, 2, median), # median
   Mean = apply(validationData, 2, mean), # mean
   Q3 = apply(validationData, 2, quantile, 3/4), # Third quartile
   Max = apply(validationData, 2, max) # Maximum
 )
 
 

 trainingData$BUILDINGID <- as.factor(trainingData$BUILDINGID)
 trainingData$FLOOR <- as.factor(trainingData$FLOOR)
 
 validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)
 validationData$FLOOR <- as.factor(validationData$FLOOR)
 
 
 #change unix time variable to actual datetime
 trainingData$DateTime <- anytime(trainingData$TIMESTAMP)
 validationData$DateTime <- anytime(validationData$TIMESTAMP)
 
 
 ##create a only-features data frame with only the WAP values
 trainingData1 <- trainingData[,1:520]
 trainingData1 <- apply(trainingData1, 2, as.numeric)
 trainingData2 <- trainingData[,521:530]
 #
 validationData1 <- validationData[,1:520]
 validationData1 <- apply(validationData1, 2, as.numeric)
 validationData2 <- validationData[,521:530]
#######create a subset
 trainingData1 <- trainingData1[1:100,]
 
 ########Delete Rows with no variance 
 trainingData2 <- trainingData2[-which(apply(trainingData1, 1, var)==0),]
 trainingData1 <- trainingData1[-which(apply(trainingData1, 1, var)==0),]
 
 
 
###Delete cols with no variance for train data and validation data 
 which(apply(trainingData1, 2, var) == 0)
 validationData1 <- validationData1[, - as.numeric(which(apply(trainingData1, 2, var) == 0))]
 trainingData1 <- trainingData1[ , - as.numeric(which(apply(trainingData1, 2, var) == 0))]
 
 #### check if both training and validation data has same colnames 
 all.equal(colnames(new_trainingData1),
           colnames(new_validationData1))#TRUE
 
 #### replece value 100 with -106 to be considered low signal
 trainingData1[trainingData1==100] <- -106
 validationData1[validationData1==100] <- -106
 
 
 
 ##Normlize training data and validation data
 trainingData1 <- as.data.frame(t(apply(trainingData1, 1, 
                                        function(x) (x - min(x))/(max(x)-min(x)))))

 validationData1 <- as.data.frame(t(apply(validationData1, 1, 
                                        function(x) (x - min(x))/(max(x)-min(x)))))
 
 
 
 pMatrix_training <- as.data.frame(trainingData1)
 princ <- prcomp(pMatrix_training, scale=FALSE, center = FALSE)
 head(princ, 10)
 
 summary(princ)
 plot(princ, type="l")
 new_trainingSet <-
   as.data.frame(cbind(brandnew_trainingData1[,1:comp],
                       trainingData2))
 #
 new_validationSet <-
   as.data.frame(cbind(brandnew_validationData1[,1:comp],
                       validationData2))
 ###Save the new data
 write.csv(new_trainingSet, "new_training.csv",
           row.names = FALSE)
 write.csv(new_validationSet, "new_validation.csv",
           row.names = FALSE)
 
 
#   
# ###Values with Variance near zero are only introducing noise in  model 
#  nzv <- nearZeroVar(trainingData1,
#                     saveMetrics = TRUE)
#  head(nzv,20)
#  
#  
#  x <- 0.0100316  #that is the value of the variance from which we will discriminate.
#  dim(nzv[nzv$percentUnique > x,])
#  
#  
# ##select the WAPs that fulfill the requirement.
#  
#  colz <- c(rownames(nzv[nzv$percentUnique > x,]))
#  ##and then eliminate them in both data sets:
#    
#    new_trainingData1 <-
#    as.data.frame(trainingData1[,colz])
#  new_validationData1 <-
#    as.data.frame(validationData1[,colz])
#  remove(x)
#  remove(colz)
#  remove(nzv)
#  
# ###check if both data sets had the same transformation 
#  all.equal(colnames(new_trainingData1),
#            colnames(new_validationData1))
#  
#  
#  
#  
#  
#  
#  dim(new_trainingData1)#428 columns
#  ## [1] 19937   428
#  dim(new_validationData1)#428 colums
#  
# ## Scaling
# ## I apply the following function -min/max-min
#  
#  normalize <- function(x) { return( (x +104) / (0 + 104) ) }
#  pmatrix1 <- as.data.frame(apply(new_trainingData1,
#                                  2,
#                                  normalize))
#  pmatrix2 <- as.data.frame(apply(new_validationData1,
#                                  2, normalize))
#  
#  
#  registerDoParallel(core = 4)
#  princ <- prcomp(pmatrix1, scale=FALSE, center = FALSE)
#  head(princ, 10)
#  
#  screeplot(princ,npcs = 80)
#  plot(princ, xlab = "var")
# 
# ### How much of the Total Variance explains each component?
# variance <- princ$sdev^2/sum(princ$sdev^2)*100
# plot(variance, type = "line", col = "red") 
# 
# 
# ## apply the same transformation to validation set.
# 
# pmatrix1 <- as.matrix(pmatrix1)
# pmatrix2 <- as.matrix(pmatrix2)
# rotation <- princ$rotation
# brandnew_trainingData1 <- pmatrix1 %*% rotation
# brandnew_validationData1 <- pmatrix2 %*% rotation
# 
# all.equal(brandnew_trainingData1, princ$x)
# 
# colnames(trainingData2) <-  c("LO", "LA", "FL", "BU", "SP", "RP")
# colnames(validationData2) <- c("LO", "LA", "FL", "BU", "SP", "RP")
# comp <- 92
# new_trainingSet <-
#   as.data.frame(cbind(brandnew_trainingData1[,1:comp],
#                       trainingData2))
# #
# new_validationSet <-
#   as.data.frame(cbind(brandnew_validationData1[,1:comp],
#                       validationData2))
# ###Save the new data
# write.csv(new_trainingSet, "new_training.csv",
#           row.names = FALSE)
# write.csv(new_validationSet, "new_validation.csv",
#           row.names = FALSE)
