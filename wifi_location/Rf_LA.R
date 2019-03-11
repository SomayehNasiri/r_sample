
trainingds
validationds

train_bl1_LA <- trainingds %>% filter(trainingds$BUILDINGID==1)
validation_bl1_LA <- validationds %>% filter(validationds$BUILDINGID==1)

set.seed(456)
inTrain <- createDataPartition(y = train_bl1_LA$LATITUDE,p = .75,list = FALSE)



#############C5 Floor
train_bl1_rf_LA <- train_bl1_LA[ inTrain,]
test_bl1_rf_LA  <- train_bl1_LA[-inTrain,]
nrow(train_bl1_rf_LA)

train_bl1_rf_LA <- sample_n(train_bl1_rf_LA, 2000)


ctrl_bl1_rf_LA <- trainControl(method="repeatedcv", number=10, repeats=3)
t_bl1_rf_LA <- system.time( Fit_bl1_rf_LA <- train(LATITUDE ~ .,
                                                     data=train_bl1_rf_LA %>% 
                                                       select(starts_with("WAP"), LATITUDE),
                                                     method="rf",
                                                     trControl= ctrl_bl1_rf_LA,
                                                     na.action = na.omit) 
                             
)

###Prediction for test set of sample partition
predict_bl1_rf_LA <- predict(Fit_bl1_rf_LA, test_bl1_rf_LA, level = .95)

###Prediction for validation data
predict_validation_bl1_rf_LA <- predict(Fit_bl1_rf_LA, validation_bl1_LA, level = .95)
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



