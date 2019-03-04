library(prophet)

ph_ds <- select(fullDs,Date,Global_active_power)
ph_ds$Date <- as.Date(ph_ds$Date)

ph_ds <- filter(ph_ds,(year(ph_ds$Date)) !=  2006)

ph_ds <- group_by(ph_ds,ph_ds$Date)

#REDUCING DATA SET SIZE BY USMeanSm3ING MEAN INSTEAD OF INDIVIDUAL OBSERVATIONS
ph_ds <- summarise(ph_ds, MeanGAP = mean(Global_active_power, na.rm = TRUE))
colnames(ph_ds)[1] <-"dateCol"

ds <- ph_ds$dateCol
y <- ph_ds$MeanGAP

qplot(ds,y,data=ph_ds)
df <- data.frame(ds,y)
m <- prophet(df)

future <- make_future_dataframe(m,periods = 365)
tail(future)
forcast <- predict(m,future)
head(forcast[c('ds','yhat','yhat_lower','yhat_upper')])
tail(forcast[c('ds','yhat','yhat_lower','yhat_upper')])
plot(m,forcast)+add_changepoints_to_plot(m)

prophet_plot_components(m,forcast)
plot.prophet(forcast)
plot_forecast_component(m, forcast, "yhat", uncertainty = TRUE,
                        plot_cap = FALSE)



plot(m)
add_changepoints_to_plot(m, threshold = 0.01, cp_color = "red",
                         cp_linetype = "dashed", trend = TRUE)
######save forcasted values in data frame

ph_forcasted_df <- forcast[c('ds','yhat')]
ph_forcasted_df_2011 <- ph_forcasted_df %>% filter(year(ph_forcasted_df$ds)==2011)
sum(ph_forcasted_df_2011$yhat)/NROW(ph_forcasted_df_2011$yhat)
##Average annual GAP result= 1.058294




######### Prophet with submeter1
ph_ds_sm1 <- select(fullDs,Date,Sub_metering_1)
ph_ds_sm1$Date <- as.POSIXct(ph_ds_sm1$Date, "%Y/%m/%d")
ph_ds_sm1 <- ph_ds_sm1  %>% filter(year(ph_ds_sm1$Date) != 2006 )


colnames(ph_ds_sm1)[1] <-"dateCol"
ph_ds_sm1 <- group_by(ph_ds_sm1,ph_ds_sm1$dateCol)

#REDUCING DATA SET SIZE BY USING MEAN INSTEAD OF INDIVIDUAL OBSERVATIONS
ph_ds_sm1 <- summarise(ph_ds_sm1, MeanSm1 = mean(Sub_metering_1, na.rm = TRUE))

colnames(ph_ds_sm1)[1] <-"dateCol"
ds <- ph_ds_sm1$dateCol
y <- ph_ds_sm1$MeanSm1

df <- data.frame(ds,y)
m <- prophet(df)
future <- make_future_dataframe(m,periods = 365)
tail(future)
forcast <- predict(m,future)
###########save forcasted results data frame
ph_forcasted_df_sm1 <- forcast[c('ds','yhat')]
ph_forcasted_df_2011_sm1 <- ph_forcasted_df_sm1 %>% filter(year(ph_forcasted_df_sm1$ds)==2011)
sum(ph_forcasted_df_2011_sm1$yhat)/NROW(ph_forcasted_df_2011_sm1$yhat)
##Average annual SM1 result=0.9605941

######### Prophet with submeter2
ph_ds_sm2 <- select(fullDs,Date,Sub_metering_2)
ph_ds_sm2$Date <- as.POSIXct(ph_ds_sm2$Date, "%Y/%m/%d")
ph_ds_sm2 <- ph_ds_sm2  %>% filter(year(ph_ds_sm2$Date) != 2006 )


colnames(ph_ds_sm2)[1] <-"dateCol"
ph_ds_sm2 <- group_by(ph_ds_sm2,ph_ds_sm2$dateCol)

#REDUCING DATA SET SIZE BY USING MEAN INSTEAD OF INDIVIDUAL OBSERVATIONS
ph_ds_sm2 <- summarise(ph_ds_sm2, MeanSm2 = mean(Sub_metering_2, na.rm = TRUE))



ds <- ph_ds_sm2$dateCol
y <- ph_ds_sm2$MeanSm2

df <- data.frame(ds,y)
m <- prophet(df)
future <- make_future_dataframe(m,periods = 365)
tail(future)
forcast <- predict(m,future)
###########save forcasted results data frame
ph_forcasted_df_sm2 <- forcast[c('ds','yhat')]
ph_forcasted_df_2011 <- ph_forcasted_df_sm2 %>% filter(year(ph_forcasted_df_sm2$ds)==2011)
sum(ph_forcasted_df_2011$yhat)/NROW(ph_forcasted_df_2011$yhat)
###average annual sm2 result=0.9305483

######### Prophet with submeter3
ph_ds_sm3 <- select(fullDs,Date,Sub_metering_3)
ph_ds_sm3$Date <- as.POSIXct(ph_ds_sm3$Date, "%Y/%m/%d")
ph_ds_sm3 <- ph_ds_sm3  %>% filter(year(ph_ds_sm3$Date) != 2006 )


colnames(ph_ds_sm3)[1] <-"dateCol"
ph_ds_sm3 <- group_by(ph_ds_sm3,ph_ds_sm3$dateCol)

#REDUCING DATA SET SIZE BY USING MEAN INSTEAD OF INDIVIDUAL OBSERVATIONS
ph_ds_sm3 <- summarise(ph_ds_sm3, MeanSm3 = mean(Sub_metering_3, na.rm = TRUE))


ds <- ph_ds_sm3$dateCol
y <- ph_ds_sm3$MeanSm3

df <- data.frame(ds,y)
m <- prophet(df)
future <- make_future_dataframe(m,periods = 60)
tail(future)

forcast <- predict(m,future)

plot(m,forcast)

###########save forcasted results data frame
ph_forcasted_df_sm3 <- forcast[c('ds','yhat')]
ph_forcasted_df_2011_sm3 <- ph_forcasted_df_sm3 %>% filter(year(ph_forcasted_df_sm3$ds)==2011)
sum(ph_forcasted_df_2011_sm3$yhat)/NROW(ph_forcasted_df_2011_sm3$yhat)
##Average annual SM3 result= 6.87044
##Average annual SM3 result= 8.495229


######### Prophet with submeter4
ph_ds_sm4 <- select(fullDs,Date,Sub_metering_4)
ph_ds_sm4 <- ph_ds_sm4  %>% filter(year(ph_ds_sm4$Date) != 2006 )
#ph_ds_sm4$Date <- as.POSIXct(ph_ds_sm4$Date, "%Y/%m/%d")

colnames(ph_ds_sm4)[1] <-"dateCol"
ph_ds_sm4 <- group_by(ph_ds_sm4,ph_ds_sm4$dateCol)

#REDUCING DATA SET SIZE BY USING MEAN INSTEAD OF INDIVIDUAL OBSERVATIONS
ph_ds_sm4 <- summarise(ph_ds_sm4, MeanSm4 = mean(Sub_metering_4, na.rm = TRUE))


ds <- ph_ds_sm4$dateCol
y <- ph_ds_sm4$MeanSm4

df <- data.frame(ds,y)
m <- prophet(df)
future <- make_future_dataframe(m,periods = 365)
tail(future)
forcast <- predict(m,future)
###########save forcasted results data frame
ph_forcasted_df_sm4 <- forcast[c('ds','yhat')]
ph_forcasted_df_2011_sm4 <- ph_forcasted_df_sm4 %>% filter(year(ph_forcasted_df_sm4$ds)==2011)
sum(ph_forcasted_df_2011_sm4$yhat)/NROW(ph_forcasted_df_2011_sm4$yhat)
##Average annual SM4 result= 9.032619


predicted_ph_sm <- c(0.9671438,0.9305483,6.87044,9.032619)

###########
m <- prophet(weekly.seasonality=FALSE)
m <- add_seasonality(m, name='monthly', period=30.5, fourier.order=5)
m <- fit.prophet(m, df)
forecast <- predict(m, future)
prophet_plot_components(m, forecast)


future <- make_future_dataframe(m,periods = 365)
tail(future)
forcast <- predict(m,future)
head(forcast[c('ds','yhat','yhat_lower','yhat_upper')])
tail(forcast[c('ds','yhat','yhat_lower','yhat_upper')])
plot(m,forcast)+add_changepoints_to_plot(m)

#### plot model and predicted values in detailed way
####***************
dyplot.prophet(m,forcast)
plot_forecast_component(m, forcast, "yhat", uncertainty = TRUE,
                        plot_cap = FALSE)

###uncertainaty seasonality
m <- prophet(df, mcmc.samples = 300)
forecast <- predict(m, future)
prophet_plot_components(m, forecast)

# R select ing a part of data from
forecast %>% 
  select(ds, yhat) %>% 
  filter(year(ds)==2011 & month(ds)==1) %>%
  tail(40)
