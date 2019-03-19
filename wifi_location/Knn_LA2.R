
new_trainingSet 
new_validationSet

######### 00000000 Building 0
train_bl0_LA <- new_trainingSet %>% filter(new_trainingSet$BUILDINGID==0)
validation_bl0_LA <- new_validationSet %>% filter(new_validationSet$BUILDINGID==0)

set.seed(456)
inTrain <- createDataPartition(y = train_bl0_LA$LATITUDE,p = .75,list = FALSE)

all.equal(colnames(train_bl0_LA),colnames(validation_bl0_LA))#TRUE

#############
train_bl_knn_LA <- train_bl0_LA[ inTrain,]
test_bl_knn_LA  <- train_bl0_LA[-inTrain,]

#train_bl_knn_LA <- sample_n(train_bl_knn_LA, 2000)



ctrl_bl0_knn_LA <- trainControl(method="repeatedcv", number=10, repeats=3)
t_bl0_knn_LA <- system.time( Fit_bl0_knn_LA <- train(LATITUDE ~ .,
                                                     data=train_bl_knn_LA %>% 
                                                       select(starts_with("WAP"), LATITUDE),
                                                     method="knn",
                                                     trControl= ctrl_bl0_knn_LA,
                                                     na.action = na.omit) 
                             
)

###Prediction for test set of sample partition
predict_test_bl0_knn_LA <- predict(Fit_bl0_knn_LA, test_bl_knn_LA, level = .95)
postResample(predict_test_bl0_knn_LA,test_bl_knn_LA$LATITUDE)

# bl0_test_pred <- data.frame(test_bl_knn_LA$LATITUDE,predict_bl0_knn_LA)

###Prediction for train set of sample partition
predict_train_bl0_knn_LA <- predict(Fit_bl0_knn_LA, train_bl_knn_LA, level = .95)
postResample(predict_train_bl0_knn_LA,train_bl_knn_LA$LATITUDE)

# bl0_test_pred <- data.frame(test_bl_knn_LA$LATITUDE,predict_bl0_knn_LA)

###Prediction for validation data
predict_validation_bl0_knn_LA <- predict(Fit_bl0_knn_LA, validation_bl0_LA)
postResample(predict_validation_bl0_knn_LA,validation_bl0_LA$LATITUDE)

##### Make data frame of errors for Latitude of validation set
validation_real_pred_knn_bl0_la <- data.frame(real_LA=validation_bl0_LA$LATITUDE,
                                           prediced_LA=predict_validation_bl0_knn_LA,
                                           building = 0)

ggplot(data = real_pred_knn_bl0_La) +
  geom_point(aes(x = real_pred_knn_bl0_La$real_LA, y = real_pred_knn_bl0_La$predicted_LA)) +
  # geom_point(aes(x = predicted_LA, y = predicted_LO), color = 'red')+
  ggtitle("K-NN Prediction va Real Values LA,LO") +
  theme_bw()+theme(axis.line = element_line(colour = "black"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank(),
                   legend.title=element_blank(),
                   legend.key=element_blank()) #+ facet_wrap(~floor_col)




ggplot(real_pred_knn_bl0_La, aes(x=real_pred_knn_bl0_La$real_LA,
                                 y=real_pred_knn_bl0_La$predicted_LA), color='red') +
  geom_point(size=3.2, alpha = 0.4)



#########11111111 Building 1 ###########


train_bl1_LA <- new_trainingSet %>% filter(new_trainingSet$BUILDINGID==1)
validation_bl1_LA <- new_validationSet %>% filter(new_validationSet$BUILDINGID==1)

# validation_bl1_LA <- validationds %>% select(starts_with("WAP"), LATITUDE)

set.seed(456)
inTrain <- createDataPartition(y = train_bl1_LA$LATITUDE,p = .75,list = FALSE)

all.equal(colnames(train_bl1_LA),
          colnames(validation_bl1_LA))#TRUE

#############
train_bl_knn_LA <- train_bl1_LA[ inTrain,]
test_bl_knn_LA  <- train_bl1_LA[-inTrain,]


ctrl_bl1_knn_LA <- trainControl(method="repeatedcv", number=10, repeats=3,allowParallel = TRUE)
t_bl1_knn_LA <- system.time( Fit_bl1_knn_LA <- train(LATITUDE ~ .,
                                        data=train_bl_knn_LA %>% 
                                          select(starts_with("WAP"), LATITUDE),
                                        method="knn",
                                       trControl= ctrl_bl1_knn_LA,
                                        na.action = na.omit))


###Prediction for test set 
predict_bl1_knn_LA <- predict(Fit_bl1_knn_LA, test_bl_knn_LA, level = .95)
bl1_test_pred <- data.frame(test_bl_knn_LA$LATITUDE,predict_bl1_knn_LA)


postResample(predict_bl1_knn_LA,test_bl_knn_LA$LATITUDE)

###Prediction for train set 
predict_train_bl1_knn_LA <- predict(Fit_bl1_knn_LA, train_bl_knn_LA, level = .95)
postResample(predict_train_bl1_knn_LA,train_bl_knn_LA$LATITUDE)

###Prediction for validation data
predict_validation_bl1_knn_LA <- predict(Fit_bl1_knn_LA, validation_bl1_LA)
postResample(predict_validation_bl1_knn_LA,validation_bl1_LA$LATITUDE)

raw_data_bl1_la <- data.frame(real_LA_raw=validation_bl1_LA$LATITUDE,
                           prediced_LA_raw=predict_validation_bl1_knn_LA,
                           building = 1)

##### Make data frame of errors for Latitude of validation set
validation_real_pred_knn_bl1_la <- data.frame(real_LA=validation_bl1_LA$LATITUDE,
                                              prediced_LA=predict_validation_bl1_knn_LA)


# validation_bl1_LA$predicted_LA <- predict_validation_bl1_knn_LA
# validation_bl1_LA$predicted_LO <- predict_validation_bl1_knn_LO
# validation_bl1_LA$errorCol <- sqrt ((validation_bl1_LA$LATITUDE - validation_bl1_LA$predicted_LA)^2 + 
#         (validation_bl1_LA$LONGITUDE - validation_bl1_LA$predicted_LO)^2)



#save the model####
save(Fit_bl1_c5_LA, file = "results/Fit_bl1_c5_LA.rda")

#prediction with the model####


#CAPTURE Metrics of the model####
RF_FL_Summary <- capture.output(c5FitAL)
cat("Summary",RF_FL_Summary,
    file = "summary of c5FitAL.txt",
    sep = "\n",
    append =TRUE)

########## 22222 Building 2
train_bl2_LA <- new_trainingSet %>% filter(new_trainingSet$BUILDINGID==2)
validation_bl2_LA <- new_validationSet %>% filter(new_validationSet$BUILDINGID==2)

set.seed(456)
inTrain <- createDataPartition(y = train_bl2_LA$LATITUDE,p = .75 ,list = FALSE)

all.equal(colnames(train_bl2_LA),
          colnames(validation_bl2_LA))#TRUE

############
train_bl_knn_LA <- train_bl2_LA[ inTrain,]
test_bl_knn_LA  <- train_bl2_LA[-inTrain,]

#train_bl_knn_LA <- sample_n(train_bl_knn_LA, 2000)



ctrl_bl2_knn_LA <- trainControl(method="repeatedcv", number=10, repeats=3, allowParallel = TRUE)
t_bl2_knn_LA <- system.time( Fit_bl2_knn_LA <- train(LATITUDE ~ .,
                                                     data=train_bl_knn_LA %>% 
                                                       select(starts_with("WAP"), LATITUDE),
                                                     method="knn",
                                                     trControl= ctrl_bl2_knn_LA,
                                                     na.action = na.omit) 
                             
)

###Prediction for test set of sample partition
predict_test_bl2_knn_LA <- predict(Fit_bl2_knn_LA, test_bl_knn_LA)
postResample(predict_test_bl2_knn_LA,test_bl_knn_LA$LATITUDE)

###Prediction for train set of sample partition
predict_train_bl2_knn_LA <- predict(Fit_bl2_knn_LA, train_bl_knn_LA)
postResample(predict_train_bl2_knn_LA,train_bl_knn_LA$LATITUDE)



###Prediction for validation data
predict_validation_bl2_knn_LA <- predict(Fit_bl2_knn_LA, validation_bl2_LA)
postResample(predict_validation_bl2_knn_LA,validation_bl2_LA$LATITUDE)

# validation_bl1_LA$predicted_LA <- predict_validation_bl1_knn_LA
# validation_bl1_LA$predicted_LO <- predict_validation_bl1_knn_LO
# validation_bl1_LA$errorCol <- sqrt ((validation_bl1_LA$LATITUDE - validation_bl1_LA$predicted_LA)^2 + 
#         (validation_bl1_LA$LONGITUDE - validation_bl1_LA$predicted_LO)^2)

validation_bl2_LA$predicted_LA <- predict_validation_bl2_knn_LA
validation_bl2_LA$predicted_LO <- predict_validation_bl2_knn_LO
validation_bl2_LA$errorCol <- sqrt ((validation_bl2_LA$LATITUDE - validation_bl2_LA$predicted_LA)^2 +
          (validation_bl2_LA$LONGITUDE - validation_bl2_LA$predicted_LO)^2)
  
  
validation_real_pred_knn_bl2_la <- data.frame(real_LA=validation_bl2_LA$LATITUDE,
                                              prediced_LA=predict_validation_bl2_knn_LA,
                                              # prediced_LO=predict_validation_bl2_knn_LO,
                                              # real_LO=validation_bl2_LO$LONGITUDE,
                                              building = 2)
ggplot(validation_real_pred_knn_bl2_la) +
  geom_point(aes(x = prediced_LA, y = prediced_LO), color = 'black')+
  geom_point(aes(x=real_LA,
                 y=real_LO), color='red')
