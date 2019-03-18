
new_trainingSet 
new_validationSet 


############## 000000000000 Building 0 ###########

train_bl0_LO <- new_trainingSet %>% filter(new_trainingSet$BUILDINGID==0)
validation_bl0_LO <- new_validationSet %>% filter(new_validationSet$BUILDINGID==0)

set.seed(456)
inTrain <- createDataPartition(y = train_bl0_LO$LONGITUDE,p = .75,list = FALSE)

#############RF Floor
train_bl0_rf_LO <- train_bl0_LO[ inTrain,]
test_bl0_rf_LO  <- train_bl0_LO[-inTrain,]
nrow(train_bl0_rf_LO)

# train_bl0_rf_LO <- sample_n(train_bl0_rf_LO, 2000)


ctrl_bl0_rf_LO <- trainControl(method="repeatedcv", number=10, repeats=1, allowParallel = TRUE )
t_bl0_rf_LO <- system.time( Fit_bl0_rf_LO <- train(LONGITUDE ~ .,
                                                   data=train_bl0_rf_LO %>% 
                                                     select(starts_with("WAP"), LONGITUDE),
                                                   method="rf",
                                                   trControl= ctrl_bl0_rf_LO,
                                                   na.action = na.omit) 
                            
)

###Prediction for test set of sample partition
predict_bl0_rf_LO <- predict(Fit_bl0_rf_LO, train_bl0_rf_LO)
postResample(predict_bl0_rf_LO,train_bl0_rf_LO$LONGITUDE)
###Prediction for validation data
predict_validation_bl0_rf_LO <- predict(Fit_bl0_rf_LO, validation_bl0_LO)
postResample(predict_validation_bl0_rf_LO,validation_bl0_LO$LONGITUDE)
####make data frame with real and predicted values
real_pred_rf_bl0_LO <- data.frame(real_LO=validation_bl0_LO$LOTITUDE,
                                  predicted_LO=predict_validation_bl0_rf_LO)

#######  11111111111 Building 1 #########




train_bl1_LO <- new_trainingSet %>% filter(new_trainingSet$BUILDINGID==1)
validation_bl1_LO <- new_validationSet %>% filter(new_validationSet$BUILDINGID==1)

set.seed(456)
inTrain <- createDataPartition(y = train_bl1_LO$LONGITUDE,p = .75,list = FALSE)



############  Floor
train_bl1_rf_LO <- train_bl1_LO[ inTrain,]
test_bl1_rf_LO  <- train_bl1_LO[-inTrain,]
nrow(train_bl1_rf_LO)

# train_bl1_rf_LO <- sample_n(train_bl1_rf_LO, 2000)


ctrl_bl1_rf_LO <- trainControl(method="repeatedcv", number=10, repeats=1, allowParallel = TRUE )
t_bl1_rf_LO <- system.time( Fit_bl1_rf_LO <- train(LONGITUDE ~ .,
                                                   data=train_bl1_rf_LO %>% 
                                                     select(starts_with("WAP"), LONGITUDE),
                                                   method="rf",
                                                   trControl= ctrl_bl1_rf_LO,
                                                   na.action = na.omit) 
                            
)

###Prediction for test set of sample partition
predict_bl1_rf_LO <- predict(Fit_bl1_rf_LO, train_bl1_rf_LO)
postResample(predict_bl1_rf_LO,train_bl1_rf_LO$LONGITUDE)
###Prediction for validation data
predict_validation_bl1_rf_LO <- predict(Fit_bl1_rf_LO, validation_bl1_LO)
postResample(predict_validation_bl1_rf_LO,validation_bl1_LO$LONGITUDE)
####make data frame with real and predicted values
real_pred_rf_bl1_LO <- data.frame(real_LO=validation_bl1_LO$LOTITUDE,
                                  predicted_LO=predict_validation_bl1_rf_LO)




plot(head( varImp(Fit_bl1_rf_LO),20))


#save the model####
save(Fit_bl1_rf_LO, file = "results/Fit_bl1_c5_LO_p1.rda")

#prediction with the model####


Fit_bl1_c5_LO_Summary <- capture.output(Fit_bl1_rf_LO)
cat("Summary", Fit_bl1_c5_LO_Summary,
    file = "summary of Fit_bl1_c5_LO_p1.txt",
    sep = "\n",
    append = TRUE)

############## 000000000000 Building 0 ###########

train_bl0_LO <- new_trainingSet %>% filter(new_trainingSet$BUILDINGID==0)
validation_bl0_LO <- new_validationSet %>% filter(new_validationSet$BUILDINGID==0)

set.seed(456)
inTrain <- createDataPartition(y = train_bl0_LO$LONGITUDE,p = .75,list = FALSE)

#############RF Floor
train_bl0_rf_LO <- train_bl0_LO[ inTrain,]
test_bl0_rf_LO  <- train_bl0_LO[-inTrain,]
nrow(train_bl0_rf_LO)

# train_bl0_rf_LO <- sample_n(train_bl0_rf_LO, 2000)


ctrl_bl0_rf_LO <- trainControl(method="repeatedcv", number=10, repeats=1, allowParallel = TRUE )
t_bl0_rf_LO <- system.time( Fit_bl0_rf_LO <- train(LONGITUDE ~ .,
                                                   data=train_bl0_rf_LO %>% 
                                                     select(starts_with("WAP"), LONGITUDE),
                                                   method="rf",
                                                   trControl= ctrl_bl0_rf_LO,
                                                   na.action = na.omit) 
                            
)

###Prediction for test set of sample partition
predict_bl0_rf_LO <- predict(Fit_bl0_rf_LO, train_bl0_rf_LO)
postResample(predict_bl0_rf_LO,train_bl0_rf_LO$LONGITUDE)
###Prediction for validation data
predict_validation_bl0_rf_LO <- predict(Fit_bl0_rf_LO, validation_bl0_LO)
postResample(predict_validation_bl0_rf_LO,validation_bl0_LO$LONGITUDE)
####make data frame with real and predicted values
real_pred_rf_bl0_LO <- data.frame(real_LO=validation_bl0_LO$LOTITUDE,
                                  predicted_LO=predict_validation_bl0_rf_LO)


############## 22222222222 Building 2 ###########

train_bl2_LO <- new_trainingSet %>% filter(new_trainingSet$BUILDINGID==2)
validation_bl2_LO <- new_validationSet %>% filter(new_validationSet$BUILDINGID==2)

set.seed(456)
inTrain <- createDataPartition(y = train_bl2_LO$LONGITUDE,p = .75,list = FALSE)

#############RF Floor
train_bl2_rf_LO <- train_bl2_LO[ inTrain,]
test_bl2_rf_LO  <- train_bl2_LO[-inTrain,]
nrow(train_bl2_rf_LO)

# train_bl2_rf_LO <- sample_n(train_bl2_rf_LO, 2000)


ctrl_bl2_rf_LO <- trainControl(method="repeatedcv", number=10, repeats=1, allowParallel = TRUE )
t_bl2_rf_LO <- system.time( Fit_bl2_rf_LO <- train(LONGITUDE ~ .,
                                                   data=train_bl2_rf_LO %>% 
                                                     select(starts_with("WAP"), LONGITUDE),
                                                   method="rf",
                                                   trControl= ctrl_bl2_rf_LO,
                                                   na.action = na.omit) 
                            
)

###Prediction for test set of sample partition
predict_bl2_rf_LO <- predict(Fit_bl2_rf_LO, train_bl2_rf_LO)
postResample(predict_bl2_rf_LO,train_bl2_rf_LO$LONGITUDE)
###Prediction for validation data
predict_validation_bl2_rf_LO <- predict(Fit_bl2_rf_LO, validation_bl2_LO)
postResample(predict_validation_bl2_rf_LO,validation_bl2_LO$LONGITUDE)
####make data frame with real and predicted values
real_pred_rf_bl2_LO <- data.frame(real_LO=validation_bl2_LO$LOTITUDE,
                                  predicted_LO=predict_validation_bl2_rf_LO)



