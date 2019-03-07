



real_pred_rf_bl1_LO_LA <- data.frame(real_LA=validation_bl1_LA$LATITUDE,
                                     predicted_LA=predict_validation_bl1_rf_LA,
                                     real_LO=validation_bl1_LO$LONGITUDE,
                                     predicted_LO=predict_validation_bl1_rf_LO,
                                     floor_col=validation_bl1_LA$FLOOR)

validation_bl1_LO

ggplot(data = real_pred_rf_bl1_LO_LA) +
  geom_point(aes(x = real_LA, y = real_LO)) +
  geom_point(aes(x = predicted_LA, y = predicted_LO), color = 'red', alpha=0.6)+
ggtitle("Random Forest Prediction vs Real Values LA,LO")+
  theme_bw()+theme(axis.line = element_line(colour = "black"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank(),
                   legend.title=element_blank(),
                   legend.key=element_blank())+ facet_wrap(~floor_col)




ggplot(real_pred_rf_bl1_La, aes(x=real_pred_rf_bl1_La$real_LA,
                                y=real_pred_rf_bl1_La$predicted_LA)) +
  geom_point(size=3.2, alpha = 0.4)

xyplot(real_pred_rf_bl1_La$predicted_LA ~ real_pred_rf_bl1_La$real_LA, data = real_pred_rf_bl1_La, 
       auto.key = TRUE, type = c("p","r"), col.line = "red" )






C5_AL_ROC <- roc(test_bl_c5$FLOOR ,RF_FL_predic[,"Down"], levels = rev(test_bl_c5$FLOOR))

plot(C5_AL_ROC, type="S", print.thres= 0.5)

plot(C5_AL_predic,test_bl_c5$FLOOR)


real_predicred_Al <- data.frame(Predicted_Floor=C5_AL_predic,Real_Floor=test_bl_c5$FLOOR)

ggplot()