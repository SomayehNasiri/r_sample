
new_trainingSet 
new_validationSet 


############## 000000000000 Building 0 ###########

train_bl0_LA <- new_trainingSet %>% filter(new_trainingSet$BUILDINGID==0)
validation_bl0_LA <- new_validationSet %>% filter(new_validationSet$BUILDINGID==0)

set.seed(456)
inTrain <- createDataPartition(y = train_bl0_LA$LATITUDE,p = .75,list = FALSE)

#############RF FLAor
train_bl0_rf_LA <- train_bl0_LA[ inTrain,]
test_bl0_rf_LA  <- train_bl0_LA[-inTrain,]
nrow(train_bl0_rf_LA)

# train_bl0_rf_LA <- sample_n(train_bl0_rf_LA, 2000)


ctrl_bl0_rf_LA <- trainControl(method="repeatedcv", number=10, repeats=1, allowParallel = TRUE )
t_bl0_rf_LA <- system.time( Fit_bl0_rf_LA <- train(LATITUDE ~ .,
                                                   data=train_bl0_rf_LA %>% 
                                                     select(starts_with("WAP"), LATITUDE),
                                                   method="rf",
                                                   trControl= ctrl_bl0_rf_LA,
                                                   na.action = na.omit) 
                            
)

###Prediction for test set of sample partition
predict_bl0_rf_LA <- predict(Fit_bl0_rf_LA, train_bl0_rf_LA)
postResample(predict_bl0_rf_LA,train_bl0_rf_LA$LATITUDE)
###Prediction for validation data
predict_validation_bl0_rf_LA <- predict(Fit_bl0_rf_LA, validation_bl0_LA)
postResample(predict_validation_bl0_rf_LA,validation_bl0_LA$LATITUDE)
####make data frame with real and predicted values
real_pred_rf_bl0_LA <- data.frame(real_LA=validation_bl0_LA$LATITUDE,
                                  predicted_LA=predict_validation_bl0_rf_LA)
######## 11111111111 building 1 #########

train_bl1_LA <- new_trainingSet %>% filter(new_trainingSet$BUILDINGID==1)
validation_bl1_LA <- new_validationSet %>% filter(new_validationSet$BUILDINGID==1)
 
set.seed(456)
inTrain <- createDataPartition(y = train_bl1_LA$LATITUDE,p = .75,list = FALSE)



############# RF Floor
train_bl1_rf_LA <- train_bl1_LA[ inTrain,]
test_bl1_rf_LA  <- train_bl1_LA[-inTrain,]
nrow(train_bl1_rf_LA)

# train_bl1_rf_LA <- sample_n(train_bl1_rf_LA, 2000)


ctrl_bl1_rf_LA <- trainControl(method="repeatedcv", number=10, repeats=1, allowParallel = TRUE )
t_bl1_rf_LA <- system.time( Fit_bl1_rf_LA <- train(LATITUDE ~ .,
                                                     data=train_bl1_rf_LA %>% 
                                                       select(starts_with("WAP"), LATITUDE),
                                                     method="rf",
                                                     trControl= ctrl_bl1_rf_LA,
                                                     na.action = na.omit) 
                             
)

###Prediction for test set of sample partition
predict_bl1_rf_LA <- predict(Fit_bl1_rf_LA, train_bl1_rf_LA)
postResample(predict_bl1_rf_LA,train_bl1_rf_LA$LATITUDE)
###Prediction for validation data
predict_validation_bl1_rf_LA <- predict(Fit_bl1_rf_LA, validation_bl1_LA)
postResample(predict_validation_bl1_rf_LA,validation_bl1_LA$LATITUDE)
####make data frame with real and predicted values
real_pred_rf_bl1_La <- data.frame(real_LA=validation_bl1_LA$LATITUDE,
                                   predicted_LA=predict_validation_bl1_rf_LA)




plot(head( varImp(Fit_bl1_rf_LA),20))


#save the model####
save(Fit_bl1_rf_LA, file = "results/Fit_bl1_c5_LA_p1.rda")

#prediction with the model####


Fit_bl1_c5_LA_Summary <- capture.output(Fit_bl1_rf_LA)
cat("Summary", Fit_bl1_c5_LA_Summary,
    file = "summary of Fit_bl1_c5_LA_p1.txt",
    sep = "\n",
    append = TRUE)



############## 22222222222 Building 2 ###########

train_bl2_LA <- new_trainingSet %>% filter(new_trainingSet$BUILDINGID==2)
validation_bl2_LA <- new_validationSet %>% filter(new_validationSet$BUILDINGID==2)

set.seed(456)
inTrain <- createDataPartition(y = train_bl2_LA$LATITUDE,p = .75,list = FALSE)

#############RF FLAor
train_bl2_rf_LA <- train_bl2_LA[ inTrain,]
test_bl2_rf_LA  <- train_bl2_LA[-inTrain,]
nrow(train_bl2_rf_LA)

# train_bl2_rf_LA <- sample_n(train_bl2_rf_LA, 2000)


ctrl_bl2_rf_LA <- trainControl(method="repeatedcv", number=10, repeats=1, allowParallel = TRUE )
t_bl2_rf_LA <- system.time( Fit_bl2_rf_LA <- train(LATITUDE ~ .,
                                                   data=train_bl2_rf_LA %>% 
                                                     select(starts_with("WAP"), LATITUDE),
                                                   method="rf",
                                                   trControl= ctrl_bl2_rf_LA,
                                                   na.action = na.omit) 
                            
)

###Prediction for test set of sample partition
predict_bl2_rf_LA <- predict(Fit_bl2_rf_LA, train_bl2_rf_LA)
postResample(predict_bl2_rf_LA,train_bl2_rf_LA$LATITUDE)
###Prediction for validation data
predict_validation_bl2_rf_LA <- predict(Fit_bl2_rf_LA, validation_bl2_LA)
postResample(predict_validation_bl2_rf_LA,validation_bl2_LA$LATITUDE)
####make data frame with real and predicted values
real_pred_rf_bl2_LA <- data.frame(real_LA=validation_bl2_LA$LATITUDE,
                                  predicted_LA=predict_validation_bl2_rf_LA)


