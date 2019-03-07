
train_bl1_LO <- trainingData %>% filter(trainingData$BUILDINGID==1)
validation_bl1_LO <- validationData %>% filter(validationData$BUILDINGID==1)

set.seed(456)
inTrain <- createDataPartition(y = train_bl1_LO$LONGITUDE,p = .75,list = FALSE)



#############RF LONGITUDE
train_bl1_rf_LO <- train_bl1_LO[ inTrain,]
test_bl1_rf_LO  <- train_bl1_LO[-inTrain,]
nrow(train_bl1_rf_LO)
train_bl1_rf_LO <- sample_n(train_bl1_rf_LO, 2000)


ctrl_bl1_rf_LO <- trainControl(method="repeatedcv", number=10, repeats=3)
t_bl1_rf_LA <- system.time( Fit_bl1_rf_LO <- train(LONGITUDE ~ .,
                                                     data=train_bl1_rf_LO %>% 
                                                       select(starts_with("WAP"), LONGITUDE),
                                                     method="rf",
                                                  
                                                     trControl= ctrl_bl1_rf_LO,
                                                     na.action = na.omit) 
                             
)

###Prediction for test set of sample partition
predict_bl1_rf_LO <- predict(Fit_bl1_rf_LO, test_bl1_rf_LO, level = .95)

###Prediction for validation data
predict_validation_bl1_rf_LO <- predict(Fit_bl1_rf_LO, validation_bl1_LO, level = .95)


