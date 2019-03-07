
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
  
