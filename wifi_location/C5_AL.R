

trainingData
validationData

library(doParallel)

registerDoParallel(cores =3)#to speed up process
R_MAX_MEM_SIZE <- memory.lmit(size = 40000)#Increase memory limit


 train_bl_1 <- trainingds %>% filter(trainingds$BUILDINGID==1)
 validation_bl_1 <- validationds %>% filter(validationds$BUILDINGID==1)

 
 set.seed(601)
 inTrain <- createDataPartition(y = train_bl_1$FLOOR,p = .75,list = FALSE)
 
 

 #############C5 Floor
 train_bl_c5 <- train_bl_1[ inTrain,]
 test_bl_c5  <- train_bl_1[-inTrain,]
 nrow(train_bl_c5)
 
 ctrl_bl_c5 <- trainControl(method="repeatedcv", number=10, repeats=1)
 tc5_AL <- system.time( c5FitAL <- train(FLOOR ~ .,
                   data=train_bl_c5 %>% 
                     select(starts_with("WAP"), FLOOR),
                   
                   method="C5.0",
                   preProcess=c("scale","center"),
                   trControl= ctrl_bl_c5,
                   na.action = na.omit) 

)

 #save the model####
 save(c5FitAL, file = "c5FitAL.rda")
 
 #prediction with the model####
 C5_AL_predic <- predict(c5FitAL, test_bl_c5, level = .95)
 
 #CAPTURE Metrics of the model####
 RF_FL_Summary <- capture.output(c5FitAL)
 cat("Summary",RF_FL_Summary,
     file = "summary of c5FitAL.txt",
     sep = "\n",
     append =TRUE)
 #confusionMatrix
 caret :: confusionMatrix(C5_AL_predic,test_bl_c5$FLOOR)
 
 C5_AL_predic <- predict(c5FitAL, test_bl_c5, level = .95)
 C5_AL_predic_validation <- predict(c5FitAL, validation_bl_1, level = .95)
 caret :: confusionMatrix(C5_AL_predic_validation,validation_bl_1$FLOOR)
 
 
 
 
 
 
 
 plot(varImp(c5FitAL))
 C5_AL_ROC <- roc(test_bl_c5$FLOOR ,RF_FL_predic[,"Down"], levels = rev(test_bl_c5$FLOOR))
 
 plot(C5_AL_ROC, type="S", print.thres= 0.5)
 
 plot(C5_AL_predic,test_bl_c5$FLOOR)
 
 
real_predicred_Al <- data.frame(Predicted_Floor=C5_AL_predic,Real_Floor=test_bl_c5$FLOOR)
 
ggplot()

 ####Prediction for validation data
 
 predict(C5_AL_predic,test_bl_1$FLOOR)
 ########KNN Floor
 train_bl_knn <- train_bl_1[ inTrain,]
 test_bl_knn  <- train_bl_1[-inTrain,]
 nrow(train_bl_knn)
 
 ctrl_bl_c5 <- trainControl(method="repeatedcv", number=10, repeats=1)
 