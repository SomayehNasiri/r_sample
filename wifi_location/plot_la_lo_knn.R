
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

ggplot(error_validation_allb , aes(x=real_LA, y=real_LO )) 
 + geom_density(aes(x =LO_error, color=building ))

ggplot(error_validation_allb, aes(x = real_LA, y = real_LO)) +  geom_point() 
+
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = error_validation_allb$prediced_LA, yend =error_validation_allb$real_LO ), alpha = .2) +  # alpha to fade lines
  geom_point() 
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()  # 

  ggplot(error_validation_allb, aes(x =errorCol)) +  geom_histogram(bins = 30) 
  
  
  
  # COLOR
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
  