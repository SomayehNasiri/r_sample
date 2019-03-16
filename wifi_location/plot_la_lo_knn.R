
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
