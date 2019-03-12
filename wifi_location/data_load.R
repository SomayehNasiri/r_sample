
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
 trainingData$RELATIVEPOSITION <- as.factor(trainingData$RELATIVEPOSITION)
 trainingData$SPACEID <- as.factor( trainingData$SPACEID)
 
 validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)
 validationData$FLOOR <- as.factor(validationData$FLOOR)
 validationData$RELATIVEPOSITION <- as.factor(validationData$RELATIVEPOSITION)
 validationData$SPACEID <- as.factor( validationData$SPACEID)
 
 
 #change unix time variable to actual datetime
 trainingData$DateTime <- anytime(trainingData$TIMESTAMP)
 validationData$DateTime <- anytime(validationData$TIMESTAMP)
 
 
 # wifi_trainData$WAP_num <- apply(wifi_trainData[,1:520], 1,
 #                                 function(x) length(which(!is.na(x))))
 trainingData$WAP_num <- apply(trainingData[,1:520], 1,
                                 function(x) length(which(!(x==100))))
 validationData$WAP_num <- apply(validationData[,1:520], 1,
                               function(x) length(which(!(x==100))))
 ####-Filter on hallway positions
 trainingData <- filter(trainingData, 
                        trainingData$RELATIVEPOSITION==2)
 
 ##create a only-features data frame with only the WAP values
 trainingData1 <- trainingData[,1:520]
 trainingData1 <- apply(trainingData1, 2, as.numeric)
 trainingData2 <- trainingData[,521:531]
 #
 validationData1 <- validationData[,1:520]
 validationData1 <- apply(validationData1, 2, as.numeric)
 validationData2 <- validationData[,521:531]
# #######create a subset for my tests
#  
#  xData <- trainingData1[1:200,200:420]
#  xData$WAP_num1 <- apply( xData[,1:221] , 1,
#                                  function(x) length(which((x == -106))))
#  vv <- xData[35,1:221]
#  
# dim(xData)
 #### replece value less than -60 with -107 to be considered low signal
 trainingData1[trainingData1 < -90] <- -106
  validationData1[validationData1 < -90 ] <- -106
 
 
 #### replece value 100 with -106 to be considered low signal
 trainingData1[trainingData1==100] <- -106
 validationData1[validationData1==100] <- -106
 
 
  ########Delete Rows with no variance for training data
 trainingData2 <- trainingData2[-which(apply(trainingData1, 1, var)==0),]
 trainingData1 <- trainingData1[-which(apply(trainingData1, 1, var)==0),]
 

 # ########Delete Rows with no variance for validation data
 # 
 # validationData2 <- validationData2[-which(apply(validationData1, 1, var)==0),]
 # validationData1 <- validationData1[-which(apply(validationData1, 1, var)==0),]

 # xx <- data.frame(validationData1[-which(apply(validationData1, 1, var)==0),])
 


###Delete cols with no variance for train data and validation data 
 
## aa <- as.data.frame( which(apply(trainingData1, 2, var) == 0))
 validationData1 <- validationData1[, - as.numeric(which(apply(trainingData1, 2, var) == 0))]
 trainingData1 <- trainingData1[ , - as.numeric(which(apply(trainingData1, 2, var) == 0))]
 
 #### check if both training and validation data has same colnames 
 all.equal(colnames(trainingData1),
           colnames(validationData1))#TRUE
 
 
 
 
 
 ####Create full data set with no normlization
cnt <-  ncol(trainingData1)
  full_trainingSet <-
   as.data.frame(cbind(trainingData1[,1:cnt],
                       trainingData2))
  full_validationSet <-
    as.data.frame(cbind(validationData1[,1:cnt],
                        validationData2))
 
 
  
  ##Normlize training data and validation data
  trainingData1 <- as.data.frame(t(apply(trainingData1, 1, 
                                    function(x) (x - min(x))/(max(x)-min(x)))))
  
  validationData1 <- as.data.frame(t(apply(validationData1, 1, 
                                    function(x) (x - min(x))/(max(x)-min(x)))))
  

  comp <- ncol(trainingData1)  
  new_trainingSet <-
    as.data.frame(cbind(trainingData1[,1:comp],
                        trainingData2))
  
  new_validationSet <-
    as.data.frame(cbind(validationData1[,1:comp],
                        validationData2))
  
 
  ###Save the new data
  write.csv(new_trainingSet, "data/new_training.csv",
            row.names = FALSE)
  write.csv(new_validationSet, "data/new_validation.csv",
            row.names = FALSE)
  
  
  #####################Extra prepration 
  #######Near Zero variables
  ##It is not making any difference because
  ##I have already removed cols and rows with no variance
  
 nzv <- nearZeroVar(trainingData1, saveMetrics = TRUE)
 print(paste(('Range:'), (range(nzv$percentUnique))))
 head(nzv)
 

  x <- 0.0100316  #that is the value of the variance from which we will discriminate.
 dim(nzv[nzv$percentUnique > x,]) ##465
  colz <- c(rownames(nzv[nzv$percentUnique > x,]))
 # 
 new_trainingData1 <- as.data.frame(trainingData1[,colz])
 new_validationData1 <- as.data.frame(validationData1[,colz])
 
 remove(x)
 remove(colz)
 remove(nzv)
 
 all.equal(colnames(new_trainingData1),
           colnames(new_validationData1))#TRUE
 
 

 
 
 registerDoParallel(core = 4)
 princ <- prcomp(pmatrix1, scale=FALSE, center = FALSE)
 head(princ, 10)
 
 
 pmatrix1 <- as.matrix(pmatrix1)
 pmatrix2 <- as.matrix(pmatrix2)
 rotation <- princ$rotation
 brandnew_trainingData1 <- pmatrix1 %*% rotation
 brandnew_validationData1 <- pmatrix2 %*% rotation
 
 all.equal(brandnew_trainingData1, princ$x)
 
 
 colnames(trainingData2) <-  c("LO", "LA", "FL", "BU", "SP", "RP")
 colnames(validationData2) <- c("LO", "LA", "FL", "BU", "SP", "RP")
 comp <- 120
 new_trainingSet <-
   as.data.frame(cbind(brandnew_trainingData1[,1:comp],
                       trainingData2))
 #
 new_validationSet <-
   as.data.frame(cbind(brandnew_validationData1[,1:comp],
                       validationData2))
 
 
 
 ###Save the new data
 write.csv(new_trainingSet, "data/new_training.csv",
           row.names = FALSE)
 write.csv(new_validationSet, "data/new_validation.csv",
           row.names = FALSE)
 
 
 
 