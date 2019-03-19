library(RColorBrewer)
colors = brewer.pal(8, "Dark2")


real_pred_knn_bl1_LO_LA <- data.frame(real_LA=validation_bl1_LA$LATITUDE,
                                      predicted_LA=predict_validation_bl1_knn_LA,
                                      real_LO=validation_bl1_LO$LONGITUDE,
                                      predicted_LO=predict_validation_bl1_knn_LO,
                                      floor_col=validation_bl1_LA$FLOOR)

ggplot(data = real_pred_knn_bl1_LO_LA) +
  geom_point(aes(x = real_LA, y = real_LO)) +
  geom_point(aes(x = predicted_LA, y = predicted_LO), color = 'red')+
  ggtitle("K-NN Prediction va Real Values LA,LO") +
  theme_bw()+theme(axis.line = element_line(colour = "black"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank(),
                   legend.title=element_blank(),
                   legend.key=element_blank()) + facet_wrap(~floor_col)
  

###Else
resultsVSactual  <- mutate(resultsVSactual, errorsFLOOR = PredictedFLOOR_KNN - FLOOR) 

real_pred_knn_bl2_AL$predicted_AL <- factor(real_pred_knn_bl2_AL$predicted_AL)
real_pred_knn_bl2_AL$real_AL <- factor(real_pred_knn_bl2_AL$real_AL)

 plot_ly(real_pred_knn_bl2_AL, 
                      x = ~realLA, 
                      y = ~realLO, 
                      z = ~predicted_AL, 
                      size=0.5,
                      #color = , 
                      colors = c('#f29595','#ddd264','#7adf90','#9acef4','#f09ded')) %>%
  add_markers(color = ~real_AL) %>%
  #add_markers(color = ~real_AL) %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))



#'#BF382A', '#0C4B8E'




PlotErrorFLOOR <- ggplot(resultsVSactual, aes(x = LONGITUDE, 
                                              y = LATITUDE,
                                              colour = (errorsFLOOR != 0)))+
  geom_jitter()+
  theme_classic() +
  labs(title="Error Locations",
       subtitle = "By building")

########## Plot error distribution LO
error_train_knn_LO <- rbind(train_real_pred_knn_bl0,train_real_pred_knn_bl1,train_real_pred_knn_bl2)
error_train_knn_LO$LO_error <- error_train_knn_LO$real_LO - error_train_knn_LO$prediced_LO

error_train_knn_LO$building <- as.factor(error_train_knn_LO$building) 
ggplot(error_train_knn_LO) + geom_density(aes(x =LO_error, color=building ))
densityplot(~ yield, group = site, data = barley, auto.key = TRUE)

###########

error_validation_allb_LA <- rbind(validation_real_pred_knn_bl0_la,
                                  validation_real_pred_knn_bl1_la,
                                  validation_real_pred_knn_bl2_la)

error_validation_allb_LO <- rbind(validation_real_pred_knn_bl0_lo,
                                  validation_real_pred_knn_bl1_lo,
                                  validation_real_pred_knn_bl2_lo)

error_validation_allb <- cbind(error_validation_allb_LA,error_validation_allb_LO)

error_validation_allb$la2 <-  (error_validation_allb$real_LA - error_validation_allb$prediced_LA)  ^2 

error_validation_allb$lo2 <- (error_validation_allb$real_LO - error_validation_allb$prediced_LO) ^ 2

error_validation_allb$errorCol <- sqrt(error_validation_allb$la2 + error_validation_allb$lo2)

error_validation_allb <- error_validation_allb[,-3]

ggplot(error_validation_allb , aes(x=real_LA, y=real_LO )) +
  geom_density(aes(x = LO_error, color=building ))

# ggplot(error_validation_allb, aes(x = real_LA, y = real_LO)) +  geom_point() +
# geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
# geom_segment(aes(xend = error_validation_allb$prediced_LA, yend =error_validation_allb$real_LO ), alpha = .2) 

 

  ggplot(error_validation_allb, aes(x =errorCol)) +  geom_histogram(bins = 30) 
  
  
  
  
  # High residuals (in abolsute terms) made more red on actual values.
  ggplot(error_validation_allb) +
    geom_point(aes(x=real_LA, y = real_LO), shape = 1) +
    geom_point(aes(x=prediced_LA, y = prediced_LO),color="red", shape = 1) +
    geom_segment(aes(x=real_LA, xend = prediced_LA,y=real_LO,  yend = prediced_LO), alpha = .2) +
    scale_color_continuous(low = "white", high = "red")+
    facet_wrap(~building)
    
    # > Color adjustments made here...
    geom_point(aes(color = abs(residuals))) + # Color mapped to abs(residuals)
    scale_color_continuous(low = "black", high = "red") +  # Colors to use here
    guides(color = FALSE) +  # Color legend removed
    # <
    
    geom_point(aes(y = predicted), shape = 1) +
    theme_bw()
    
    
    ############# Raw data to plot
    test_preprocessed <- test  %>% filter (test$building==1)
    processed_data_bl1 <- error_validtion_allb[,1:5]
    processed_data_bl1 <- processed_data_bl1 %>% filter (processed_data_bl1$building==1)
    processed_data_bl1 <- processed_data_bl1[,1:4]
    processed_data_bl1$is_raw <- 0
    
    raw_data_bl1 <- cbind(raw_data_bl1_la,raw_data_bl1_lo)
    raw_data_bl1 <- raw_data_bl1[,-5]
    raw_data_bl1$is_raw <- 1
    raw_data_bl1 <- raw_data_bl1[1:4]
    test_raw_data_bl1 <- raw_data_bl1
    test_raw_data_bl1$erroCol <-  format(sqrt((raw_data_bl1$prediced_LA - raw_data_bl1$real_LA)^2 )+
                                           ((raw_data_bl1$real_LO - raw_data_bl1$prediced_LO )^2 ), 3 )
    
    
    names(raw_data_bl1)[3]<-"real_LO"
    raw_processed <- rbind(processed_data_bl1,raw_data_bl1)
    
    test <- raw_processed
    raw_processed[raw_processed==0] <- 2
    #plotto compare without pre processing and after pre processing
    ggplot(raw_data_bl1) +
      geom_point(aes(x = real_LA, y = real_LO, colour = "Real Data"))+
      geom_point(aes(x=prediced_LA,y=prediced_LO, colour = "Predicted Data") )+ facet_wrap(~is_raw)+
      geom_segment(aes(x=real_LA, xend = prediced_LA,y=real_LO,  yend = prediced_LO), alpha = .2) +
      ggtitle("Predictions for Raw Data") +
      xlab("LATITUDE") + ylab("LONGTITUDE")+
      
       theme(legend.position="right")
    
    
 ############# Floor ALL buildings
    
    
floor_prediction_validation <- rbind(validation_real_pred_knn_bl0_AL,
                                         validation_real_pred_knn_bl1_AL,
                                         validation_real_pred_knn_bl2_AL)
library(broom)  
    
floor_hit <- floor_prediction_validation %>% filter(predicted_AL == real_AL)
floor_miss <-  floor_prediction_validation %>%  filter(!( predicted_AL ==real_AL))

hit_phone_count <- floor_hit %>%  group_by(phoneID) %>% summarise(count= length(phoneID))
miss_phone_count <- floor_miss %>%  group_by(phoneID) %>% summarise(count= length(phoneID))



phone_hit <- nrow(table(floor_hit$phoneID))
#-Assign new column names
names(phone_hit) <- c('PhoneID', 'Count_hit') 

#-Make data frame of counts for incorrectly classified by phoneID
phone_mis <- nrow(table(floor_miss$PHONEID))
#-Assign new column names
names(phone_mis) <- c('PhoneID', 'Count_miss')

#-Merge data frames
x <- merge(x=phone_hit, y=phone_mis, by = 'PhoneID')

#-Add column for percent missed by phoneID
y <- mutate(x, Percent_miss = round(Count_miss/(Count_miss + Count_hit)*100,3))

################ train set
floor_prediction_train <- rbind( real_pred_train_rf_bl0_AL,
                                 real_pred_train_rf_bl1_AL,
                                 real_pred_train_rf_bl2_AL)

floor_hit_t <- floor_prediction_train %>% filter(predicted_AL == real_AL)
floor_miss_t <-  floor_prediction_train %>%  filter(!( predicted_AL ==real_AL))

hit_phone_count_t <- floor_hit_t %>%  group_by(phoneID) %>% summarise(count= length(phoneID))
miss_phone_count_t <- floor_miss_t %>%  group_by(phoneID) %>% summarise(count= length(phoneID))


######## Compare errors for lm and KNN
##Lm
validation_real_pred_lm_bl0
validation_real_pred_knn_bl0 <- cbind( validation_real_pred_knn_bl0_la,validation_real_pred_knn_bl0_lo)


validation_real_pred_lm_bl0$la2 <-  (as.numeric(validation_real_pred_lm_bl0$real_LA) -
                                                 as.numeric (validation_real_pred_lm_bl0$predied_LA)) ^ 2 
validation_real_pred_lm_bl0$lo2 <- (validation_real_pred_lm_bl0$real_LO -
                                            validation_real_pred_lm_bl0$prediced_LO)  ^ 2 
validation_real_pred_lm_bl0$errorCol <- sqrt(validation_real_pred_lm_bl0$la2 + validation_real_pred_lm_bl0$lo2)
validation_real_pred_lm_bl0$model <- "lm"
### Plot error for lm
ggplot(validation_real_pred_lm_bl0, aes(x =errorCol)) +  geom_histogram(bins = 30) 

## KNN
validation_real_pred_lm_bl0
validation_real_pred_knn_bl0$la2 <-  (as.numeric(validation_real_pred_knn_bl0$real_LA) -
                                       as.numeric (validation_real_pred_knn_bl0$prediced_LA)) ^ 2 
validation_real_pred_knn_bl0$lo2 <- (validation_real_pred_knn_bl0$real_LO -
                                      validation_real_pred_knn_bl0$prediced_LO)  ^ 2 
validation_real_pred_knn_bl0$errorCol <- sqrt(validation_real_pred_knn_bl0$la2 + validation_real_pred_knn_bl0$lo2)
### Plot error for lm
validation_real_pred_knn_bl0$model <- "knn"
ggplot(validation_real_pred_knn_bl0, aes(x =errorCol)) +  geom_histogram(bins = 30) 

### Mix to data set to generate better plot
validation_real_pred_lm_bl0 <- validation_real_pred_lm_bl0 %>%  select(real_LA,predied_LA,real_LO,
                                                                       prediced_LO,errorCol)
validation_real_pred_knn_bl0 <- validation_real_pred_knn_bl0 %>%  select(real_LA,prediced_LA,real_LO,
                                                                         prediced_LO,errorCol,model)
names(validation_real_pred_lm_bl0)[2]<-"prediced_LA" 

error_knn_lm <- rbind(validation_real_pred_knn_bl0,validation_real_pred_lm_bl0)

ggplot(error_knn_lm, aes(x =errorCol, color=model)) +  geom_histogram( bins = 30) 

ggplot(error_knn_lm, aes(x=errorCol, color=model)) +
  geom_histogram(fill="white", alpha=0.3, position="identity", bins = 100) -> hist_lmKnn

plotly::ggplotly(hist_lmKnn)

ggplot(error_knn_lm, aes(x=errorCol, color=model)) +
  geom_histogram()
########## Find Data with tooo faaar errors LA LO in B1
ss <- error_validation_allb %>% filter(building == 1)

find_high_error_LA <- error_validation_allb  %>% filter(LATITUDE == 4864844)# & LONGITUDE==-7435.346 )


