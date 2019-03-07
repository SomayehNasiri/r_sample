
train_bl1_LA <- trainingData %>% filter(trainingData$BUILDINGID==1)
validation_bl1_LA <- validationData %>% filter(validationData$BUILDINGID==1)

set.seed(456)
inTrain <- createDataPartition(y = train_bl1_LA$LATITUDE,p = .75,list = FALSE)



#############C5 Floor
train_bl_knn_LA <- train_bl1_LA[ inTrain,]
test_bl_knn_LA  <- train_bl1_LA[-inTrain,]
nrow(train_bl_knn_LA)
train_bl_knn_LA <- sample_n(train_bl_knn_LA, 2000)



ctrl_bl1_knn_LA <- trainControl(method="repeatedcv", number=10, repeats=3)
t_bl1_knn_LA <- system.time( Fit_bl1_knn_LA <- train(LATITUDE ~ .,
                                        data=train_bl_knn_LA %>% 
                                          select(starts_with("WAP"), LATITUDE),
                                        method="knn",
                                       trControl= ctrl_bl1_knn_LA,
                                        na.action = na.omit) 
                       
)

###Prediction for test set of sample partition
predict_bl1_knn_LA <- predict(Fit_bl1_knn_LA, test_bl_knn_LA, level = .95)

###Prediction for validation data
predict_validation_bl1_knn_LA <- predict(Fit_bl1_knn_LA, validation_bl1_LA, level = .95)

real_pred_knn_bl1_La <- data.frame(real_LA=validation_bl1_LA$LATITUDE,
                                   predicted_LA=predict_validation_bl1_knn_AL)

ggplot(real_pred_knn_bl1_La, aes(x=real_pred_knn_bl1_La$real_LA,
                                 y=real_pred_knn_bl1_La$predicted_LA)) +
                               geom_point(size=3.2, alpha = 0.4)

xyplot(real_pred_knn_bl1_La$predicted_LA ~ real_pred_knn_bl1_La$real_LA, data = real_pred_knn_bl1_La, 
            auto.key = TRUE, type = c("p","r"), col.line = "red" )

#save the model####
save(Fit_bl1_c5_LA, file = "results/Fit_bl1_c5_LA.rda")

#prediction with the model####


#CAPTURE Metrics of the model####
RF_FL_Summary <- capture.output(c5FitAL)
cat("Summary",RF_FL_Summary,
    file = "summary of c5FitAL.txt",
    sep = "\n",
    append =TRUE)








plot(varImp(c5FitAL))
C5_AL_ROC <- roc(test_bl_c5$FLOOR ,RF_FL_predic[,"Down"], levels = rev(test_bl_c5$FLOOR))

plot(C5_AL_ROC, type="S", print.thres= 0.5)

plot(C5_AL_predic,test_bl_c5$FLOOR)


real_predicred_Al <- data.frame(Predicted_Floor=C5_AL_predic,Real_Floor=test_bl_c5$FLOOR)

ggplot()