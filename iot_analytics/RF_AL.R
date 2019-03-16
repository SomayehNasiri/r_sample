new_trainingSet 
new_validationSet 


######## Building 0000000   #################

train_bl0_AL <- new_trainingSet %>% filter(new_trainingSet$BUILDINGID==0  )
validation_bl0_AL <- new_validationSet %>% filter(new_validationSet$BUILDINGID==0)
validation_bl0_AL$FLOOR <- factor(validation_bl0_AL$FLOOR)
set.seed(456)
inTrain <- createDataPartition(y = factor(train_bl0_AL$FLOOR),p = .80,list = FALSE)

all.equal(colnames(train_bl0_AL),colnames(validation_bl0_AL))#TRUE

############
train_bl0_rf_AL <- train_bl0_AL[ inTrain,]
test_bl0_rf_AL  <- train_bl0_AL[-inTrain,]


ctrl_bl0_rf_AL <- trainControl(method="repeatedcv", number=10, repeats=3)
t_bl0_rf_AL <- system.time( Fit_bl0_rf_AL <- train(factor(FLOOR) ~ .,
                                                     data=train_bl0_rf_AL %>% 
                                                       select(starts_with("WAP"), FLOOR),
                                                     method="rf",
                                                     trControl= ctrl_bl0_rf_AL,
                                                     na.action = na.omit) )



###Prediction for test set of sample partition
predict_bl0_rf_AL <- predict(Fit_bl0_rf_AL, test_bl0_rf_AL)

caret::confusionMatrix(predict_bl0_rf_AL,factor(test_bl0_rf_AL$FLOOR))

######## MAke data frame with errors

test_real_pred_rf_bl0_AL <- data.frame( realLA= test_bl0_rf_AL$LATITUDE,
                                         realLO= test_bl0_rf_AL$LONGITUDE,
                                         user=test_bl0_rf_AL$USERID,
                                         phoneID=test_bl0_rf_AL$PHONEID,
                                         timecol = test_bl0_rf_AL$DateTime,
                                         reletiveposition=test_bl0_rf_AL$RELATIVEPOSITION,
                                         spaceId=test_bl0_rf_AL$SPACEID,
                                         timeStampCol=test_bl0_rf_AL$TIMESTAMP,
                                         
                                         real_AL=test_bl0_rf_AL$FLOOR,
                                         predicted_AL=predict_bl0_rf_AL)

test_real_pred_rf_bl0_AL$real_AL <- factor(test_real_pred_rf_bl0_AL$real_AL)

test_wrong_predictions_bl0 <- test_real_pred_rf_bl0_AL %>% filter( predicted_AL==2)

###Prediction for train set
predict_bl0_rf_AL_train <- predict(Fit_bl0_rf_AL, train_bl0_AL)
caret::confusionMatrix(predict_bl0_rf_AL_train,factor(train_bl0_AL$FLOOR))

###Prediction for validation data
predict_validation_bl0_rf_AL <- predict(Fit_bl0_rf_AL, validation_bl0_AL , level = .95)
caret::confusionMatrix(predict_validation_bl0_rf_AL,factor(validation_bl0_AL$FLOOR))

plot(varImp(Fit_bl0_rf_AL))

real_pred_rf_bl0_AL <- data.frame(realLA= train_bl0_AL$LATITUDE,
                                   realLO= train_bl0_AL$LONGITUDE,
                                   user=train_bl0_AL$USERID,
                                   phoneID=train_bl0_AL$PHONEID,
                                   timecol = train_bl0_AL$DateTime,
                                   reletiveposition=train_bl0_AL$RELATIVEPOSITION,
                                   spaceId=train_bl0_AL$SPACEID,
                                   timeStampCol=train_bl0_AL$TIMESTAMP,
                                   average=train_bl0_AL$WAP_average,
                                   real_AL=train_bl0_AL$FLOOR,
                                   predicted_AL=predict_bl0_rf_AL_train)








real_pred_rf_bl0_AL$real_AL <- factor(real_pred_rf_bl0_AL$real_AL)

wrong_predictions_bl0 <- real_pred_rf_bl0_AL %>% filter( ! (real_AL==predicted_AL))

########### filter time error
specific_df <-  train_bl0_AL %>% filter ( train_bl0_AL$BUILDINGID==0 , train_bl0_AL$USERID==1, 
                                          train_bl0_AL$PHONEID==14, train_bl0_AL$SPACEID==102,
                                          train_bl0_AL$TIMESTAMP == 1371047342   
                                          |  train_bl0_AL$TIMESTAMP ==1371047343
                                          | train_bl0_AL$TIMESTAMP ==  1371047344)


listofwaps <- which(apply(specific_df, 2, var) == 0)

specific_df <-  specific_df [ , !listofwaps ]


#########            Building 111111

train_bl1_AL <- new_trainingSet %>% filter(new_trainingSet$BUILDINGID==1)
validation_bl1_AL <- new_validationSet %>% filter(new_validationSet$BUILDINGID==1 )
validation_bl1_AL$FLOOR <- factor(validation_bl1_AL$FLOOR)

set.seed(456)
inTrain <- createDataPartition(y = factor(train_bl1_AL$FLOOR) ,p = .90,list = FALSE)

all.equal(colnames(train_bl1_AL),
          colnames(validation_bl1_AL))#TRUE

############
train_bl1_rf_AL <- train_bl1_AL[ inTrain,]
test_bl1_rf_AL  <- train_bl1_AL[-inTrain,]

#train_bl_rf_AL <- sample_n(train_bl_rf_AL, 2000)



ctrl_bl1_rf_AL <- trainControl(method="repeatedcv", number=10, repeats=3)
t_bl1_rf_AL <- system.time( Fit_bl1_rf_AL <- train(factor(FLOOR) ~ .,
                                                     data=train_bl1_rf_AL %>% 
                                                       select(starts_with("WAP"), FLOOR),
                                                     method="rf",
                                                     trControl= ctrl_bl1_rf_AL,
                                                     na.action = na.omit) 
                             
)

###Prediction for test set of sample partition
predict_bl1_rf_AL <- predict(Fit_bl1_rf_AL, test_bl1_rf_AL)
caret::confusionMatrix(predict_bl1_rf_AL,factor(test_bl1_rf_AL$FLOOR))


test_real_pred_rf_bl1_AL <- data.frame(realLA= test_bl1_rf_AL$LATITUDE,
                                        realLO= test_bl1_rf_AL$LONGITUDE,
                                        user=test_bl1_rf_AL$USERID,
                                        phoneID=test_bl1_rf_AL$PHONEID,
                                        timecol = test_bl1_rf_AL$DateTime,
                                        time_stamp = test_bl1_rf_AL$TIMESTAMP,
                                        reletiveposition=test_bl1_rf_AL$RELATIVEPOSITION,
                                        real_AL=test_bl1_rf_AL$FLOOR,
                                        predicted_AL=predict_bl1_rf_AL)



###Prediction for train set
predict_bl1_rf_AL_train <- predict(Fit_bl1_rf_AL, train_bl1_rf_AL, level = .95)
caret::confusionMatrix(predict_bl1_rf_AL_train,factor(train_bl1_rf_AL$FLOOR))

####### make data frame for errors for train set
real_pred_rf_bl1_AL <- data.frame(realLA= train_bl_rf_AL$LATITUDE,
                                   realLO= train_bl_rf_AL$LONGITUDE,
                                   user=train_bl_rf_AL$USERID,
                                   phoneID=train_bl_rf_AL$PHONEID,
                                   timecol = train_bl_rf_AL$DateTime,
                                   time_stamp = train_bl_rf_AL$TIMESTAMP,
                                   reletiveposition=train_bl_rf_AL$RELATIVEPOSITION,
                                   
                                   real_AL=train_bl_rf_AL$FLOOR,
                                   predicted_AL=predict_bl1_rf_AL_train)

real_pred_rf_bl1_AL$real_AL <- factor(real_pred_rf_bl1_AL$real_AL)
wrong_predictions <- real_pred_rf_bl1_AL %>% filter( ! (real_AL == predicted_AL))


###Prediction for validation data
predict_validation_bl1_rf_AL <- predict(Fit_bl1_rf_AL, validation_bl1_AL )
caret::confusionMatrix(factor(predict_validation_bl1_rf_AL),factor(validation_bl1_AL$FLOOR))


####### make data frame for errors for validation set
validation_real_pred_rf_bl1_AL <- data.frame(realLA= validation_bl1_AL$LATITUDE,
                                              realLO= validation_bl1_AL$LONGITUDE,
                                              user=validation_bl1_AL$USERID,
                                              phoneID=validation_bl1_AL$PHONEID,
                                              timecol = validation_bl1_AL$DateTime,
                                              time_stamp = validation_bl1_AL$TIMESTAMP,
                                              reletiveposition=validation_bl1_AL$RELATIVEPOSITION,
                                              
                                              real_AL=validation_bl1_AL$FLOOR,
                                              predicted_AL=predict_validation_bl1_rf_AL)

validation_real_pred_rf_bl1_AL$real_AL <- factor(validation_real_pred_rf_bl1_AL$real_AL)
wrong_predictions <- validation_real_pred_rf_bl1_AL %>% filter( ! (real_AL == predicted_AL))



ggplot(data = validation_real_pred_rf_bl1_AL) +
  geom_point(aes(x = realLA, y = realLO, color= as.factor(real_AL)), alpha= 0.001) +
  geom_point(aes(x = realLA, y = realLO, color = as.factor(predicted_AL)), alpha= 0.7)+
  facet_wrap(~real_AL)

ggtitle("K-NN Prediction vs Real Values LAA") 2
theme_bw()+theme(axis.line = element_line(colour = "black"),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 legend.title=element_blank(),
                 legend.key=element_blank()) #+ facet_wrap(~floor_col)



#@save the model####
save(RF_FL, file = "RF_FL.rda")
#

#@CAPTURE Metrics of the model####
RF_FL_Summary <- capture.output(RF_FL)
cat("Summary",RF_FL_Summary,
    file = "summary of RF_FL.txt",
    sep = "\n",
    append =TRUE)
#
RF_FL






######## Building 222222  ###################################
new_trainingSet 
new_validationSet 

train_bl2_AL <- new_trainingSet %>% filter(new_trainingSet$BUILDINGID==2)
validation_bl2_AL <- new_validationSet %>% filter(new_validationSet$BUILDINGID==2)
validation_bl2_AL$FLOOR <- factor(validation_bl2_AL$FLOOR)
set.seed(456)
inTrain <- createDataPartition(y = train_bl2_AL$FLOOR,p = .75,list = FALSE)

all.equal(colnames(train_bl2_AL),colnames(validation_bl2_AL))#TRUE

############
train_bl2_rf_AL <- train_bl2_AL[ inTrain,]
test_bl2_rf_AL  <- train_bl2_AL[-inTrain,]

ctrl_bl2_rf_AL <- trainControl(method="repeatedcv", number=10, repeats=3)
t_bl2_rf_AL <- system.time( Fit_bl2_rf_AL <- train(factor(FLOOR) ~ .,
                                                     data=train_bl2_rf_AL %>% 
                                                       select(starts_with("WAP"), FLOOR),
                                                     method="rf",
                                                     trControl= ctrl_bl2_rf_AL,
                                                     na.action = na.omit) 
                             
)

###Prediction for test set of sample partition
predict_bl2_rf_AL <- predict(Fit_bl2_rf_AL, test_bl2_rf_AL)
caret::confusionMatrix(predict_bl2_rf_AL,factor(test_bl2_rf_AL$FLOOR))

###Prediction for train set
predict_bl2_rf_AL_train <- predict(Fit_bl2_rf_AL, train_bl2_AL, level = .95)
caret::confusionMatrix(predict_bl2_rf_AL_train,factor(train_bl2_AL$FLOOR))
########### Make data frame from errors train set
real_pred_rf_bl2_AL <- data.frame(realLA= train_bl2_AL$LATITUDE,
                                   realLO= train_bl2_AL$LONGITUDE,
                                   user=train_bl2_AL$USERID,
                                   phoneID=train_bl2_AL$PHONEID,
                                   timecol = train_bl2_AL$DateTime,
                                   reletiveposition=train_bl2_AL$RELATIVEPOSITION,
                                   timestampcol=train_bl2_AL$TIMESTAMP,
                                   
                                   real_AL=train_bl2_AL$FLOOR,
                                   predicted_AL=predict_bl2_rf_AL_train)

real_pred_rf_bl2_AL$predicted_AL <- factor(real_pred_rf_bl2_AL$predicted_AL)

wrong_predictions_bl2 <- real_pred_rf_bl2_AL %>% filter( ! (real_AL==predicted_AL), real_AL==2)



###Prediction for validation data
predict_validation_bl2_rf_AL <- predict(Fit_bl2_rf_AL, validation_bl2_AL , level = .95)
caret::confusionMatrix(predict_validation_bl2_rf_AL,factor(validation_bl2_AL$FLOOR))


########### make data frame from erors for validation set
real_pred_rf_bl2_AL <- data.frame(realLA= train_bl2_AL$LATITUDE,
                                   realLO= train_bl2_AL$LONGITUDE,
                                   user=train_bl2_AL$USERID,
                                   phoneID=train_bl2_AL$PHONEID,
                                   timecol = train_bl2_AL$DateTime,
                                   reletiveposition=train_bl2_AL$RELATIVEPOSITION,
                                   timestampcol=train_bl2_AL$TIMESTAMP,
                                   
                                   real_AL=train_bl2_AL$FLOOR,
                                   predicted_AL=predict_bl2_rf_AL_train)

real_pred_rf_bl2_AL$predicted_AL <- factor(real_pred_rf_bl2_AL$predicted_AL)
wrong_predictions_bl2 <- real_pred_rf_bl2_AL %>% filter( ! (real_AL==predicted_AL), real_AL==2)



