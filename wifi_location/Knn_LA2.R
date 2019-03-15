
new_trainingSet 
new_validationSet

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


ctrl_bl1_knn_LA <- trainControl(method="repeatedcv", number=10, repeats=3)
t_bl1_knn_LA <- system.time( Fit_bl1_knn_LA <- train(LATITUDE ~ .,
                                        data=train_bl_knn_LA %>% 
                                          select(starts_with("WAP"), LATITUDE),
                                        method="knn",
                                       trControl= ctrl_bl1_knn_LA,
                                        na.action = na.omit) 
                       
)

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

real_pred_knn_bl1_La <- data.frame(real_LA=validation_bl1_LA$LATITUDE,
                                   predicted_LA=predict_validation_bl1_knn_LA)

ggplot(data = real_pred_knn_bl1_La) +
  geom_point(aes(x = real_pred_knn_bl1_La$real_LA, y = real_pred_knn_bl1_La$predicted_LA)) +
 # geom_point(aes(x = predicted_LA, y = predicted_LO), color = 'red')+
  ggtitle("K-NN Prediction va Real Values LA,LO") +
  theme_bw()+theme(axis.line = element_line(colour = "black"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank(),
                   legend.title=element_blank(),
                   legend.key=element_blank()) #+ facet_wrap(~floor_col)




ggplot(real_pred_knn_bl1_La, aes(x=real_pred_knn_bl1_La$real_LA,
                                 y=real_pred_knn_bl1_La$predicted_LA), color='red') +
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

########## 22222 Building 2
train_bl2_LA <- trainingds %>% filter(trainingds$BUILDINGID==2)
validation_bl2_LA <- validationds %>% filter(validationds$BULDINGID==2)

set.seed(456)
inTrain <- createDataPartition(y = train_bl2_LA$LATITUDE,p = .75,list = FALSE)

all.equal(colnames(train_bl2_LA),
          colnames(validation_bl2_LA))#TRUE

############
train_bl_knn_LA <- train_bl2_LA[ inTrain,]
test_bl_knn_LA  <- train_bl2_LA[-inTrain,]

#train_bl_knn_LA <- sample_n(train_bl_knn_LA, 2000)



ctrl_bl2_knn_LA <- trainControl(method="repeatedcv", number=10, repeats=3)
t_bl2_knn_LA <- system.time( Fit_bl2_knn_LA <- train(LATITUDE ~ .,
                                                     data=train_bl_knn_LA %>% 
                                                       select(starts_with("WAP"), LATITUDE),
                                                     method="knn",
                                                     trControl= ctrl_bl2_knn_LA,
                                                     na.action = na.omit) 
                             
)

###Prediction for test set of sample partition
predict_bl2_knn_LA <- predict(Fit_bl2_knn_LA, test_bl_knn_LA, level = .95)
bl2_test_pred <- data.frame(test_bl_knn_LA$LATITUDE,predict_bl2_knn_LA)


postResample(predict_bl2_knn_LA,test_bl_knn_LA$LATITUDE)

###Prediction for validation data
predict_validation_bl2_knn_LA <- predict(Fit_bl2_knn_LA, validation_bl2_LA)

real_pred_knn_bl2_La <- data.frame(real_LA=validation_bl2_LA$LATITUDE,
                                   predicted_LA=predict_validation_bl2_knn_LA)

ggplot(data = real_pred_knn_bl2_La) +
  geom_point(aes(x = real_pred_knn_bl2_La$real_LA, y = real_pred_knn_bl2_La$predicted_LA)) +
  # geom_point(aes(x = predicted_LA, y = predicted_LO), color = 'red')+
  ggtitle("K-NN Prediction va Real Values LA,LO") +
  theme_bw()+theme(axis.line = element_line(colour = "black"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank(),
                   legend.title=element_blank(),
                   legend.key=element_blank()) #+ facet_wrap(~floor_col)




ggplot(real_pred_knn_bl2_La, aes(x=real_pred_knn_bl2_La$real_LA,
                                 y=real_pred_knn_bl2_La$predicted_LA), color='red') +
  geom_point(size=3.2, alpha = 0.4)


######### 33333 Building 3
train_bl3_LA <- trainingds %>% filter(trainingds$BUILDINGID==3)
validation_bl3_LA <- validationds %>% filter(validationds$BUILDINGID==3)

set.seed(456)
inTrain <- createDataPartition(y = train_bl3_LA$LATITUDE,p = .75,list = FALSE)

all.equal(colnames(train_bl3_LA),
          colnames(validation_bl3_LA))#TRUE

#############
train_bl_knn_LA <- train_bl3_LA[ inTrain,]
test_bl_knn_LA  <- train_bl3_LA[-inTrain,]

#train_bl_knn_LA <- sample_n(train_bl_knn_LA, 2000)



ctrl_bl3_knn_LA <- trainControl(method="repeatedcv", number=10, repeats=3)
t_bl3_knn_LA <- system.time( Fit_bl3_knn_LA <- train(LATITUDE ~ .,
                                                     data=train_bl_knn_LA %>% 
                                                       select(starts_with("WAP"), LATITUDE),
                                                     method="knn",
                                                     trControl= ctrl_bl3_knn_LA,
                                                     na.action = na.omit) 
                             
)

###Prediction for test set of sample partition
predict_bl3_knn_LA <- predict(Fit_bl3_knn_LA, test_bl_knn_LA, level = .95)
bl3_test_pred <- data.frame(test_bl_knn_LA$LATITUDE,predict_bl3_knn_LA)


postResample(predict_bl3_knn_LA,test_bl_knn_LA$LATITUDE)

###Prediction for validation data
predict_validation_bl3_knn_LA <- predict(Fit_bl3_knn_LA, validation_bl3_LA)

real_pred_knn_bl3_La <- data.frame(real_LA=validation_bl3_LA$LATITUDE,
                                   predicted_LA=predict_validation_bl3_knn_LA)

ggplot(data = real_pred_knn_bl3_La) +
  geom_point(aes(x = real_pred_knn_bl3_La$real_LA, y = real_pred_knn_bl3_La$predicted_LA)) +
  # geom_point(aes(x = predicted_LA, y = predicted_LO), color = 'red')+
  ggtitle("K-NN Prediction va Real Values LA,LO") +
  theme_bw()+theme(axis.line = element_line(colour = "black"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank(),
                   legend.title=element_blank(),
                   legend.key=element_blank()) #+ facet_wrap(~floor_col)




ggplot(real_pred_knn_bl3_La, aes(x=real_pred_knn_bl3_La$real_LA,
                                 y=real_pred_knn_bl3_La$predicted_LA), color='red') +
  geom_point(size=3.2, alpha = 0.4)

