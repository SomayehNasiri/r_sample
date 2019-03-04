if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,RColorBrewer,RMySQL,dplyr,lubridate,ggplot2,ggfortify,plotly,forecast)


########### Charts############
## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(pc_ds, pc_ds$Weekday == "Monday" & pc_ds$Hour == 20 & pc_ds$Minute == 1)

## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1))

autoplot(tsSM3_070809weekly, colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")

## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly)
########### Forcast############
## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
####Monthly Forcast For Submeter3

fitSM3_monthly <- tslm(tsMonthly3 ~ trend + season)
summary(fitSM3_monthly)

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods
forecastfitSM3_monthly <- forecast(fitSM3_monthly, h=20)
plot(forecastfitSM3_monthly)


## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3_monthlyc <- forecast(fitSM3_monthly, h=20, level=c(80,90))
plot(forecastfitSM3_monthlyc, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time",main ="Wh & AC")
 
plot(fitSM3_monthly,forecastfitSM3_monthlyc)

m_ts_sm3 <- prophet(forecastfitSM3_monthly)


####Monthly Forcast For Submeter2    
fitSM2_monthly <- tslm(tsMonthly2 ~ trend + season)
summary(fitSM2_monthly)

## Create the forecast for sub-meter 2. Forecast ahead 20 time periods
forecastfitSM2_monthly <- forecast(fitSM2_monthly, h=20)
## Plot the forecast for sub-meter 2. 
plot(forecastfitSM2_monthly)

## Create sub-meter 2 forecast with confidence levels 80 and 90
forecastfitSM2_monthlyc <- forecast(fitSM2_monthly, h=20, level=c(80,90))
## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM2_monthlyc, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time",main = "Laundry Room")




     