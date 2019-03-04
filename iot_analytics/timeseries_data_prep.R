

###prepare data set for group by

ts_ds <- select(fullDs,Year,Month,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power,total_energy_cons)

ts_ds <- ts_ds[-1,]
###Summrise data based on Monthh
#COMPRESSING DATA BY GROUPING BY MONTH AND YEAR
ts_ds <- group_by(ts_ds, Year, Month)

#REDUCING DATA SET SIZE BY USING MEAN INSTEAD OF INDIVIDUAL OBSERVATIONS
ts_ds <- summarise(ts_ds, MeanGAP = mean(Global_active_power, na.rm = TRUE),
                   MeanTEC = mean(total_energy_cons, na.rm = TRUE),
                   MeanSubm1 = mean(Sub_metering_1, na.rm = TRUE),
                   MeanSubm2 = mean(Sub_metering_2, na.rm = TRUE),
                   MeanSubm3 = mean(Sub_metering_3, na.rm = TRUE))

ts_ds <- arrange(ts_ds, Year, Month)


tsMonthly1 <- ts(ts_ds$MeanSubm1,frequency = 12, start = c(2007,2),end = c(2010,11))
plot(tsMonthly1,col = "navy",ylab= "Watt Hours",  main = "Kitchen")

tsMonthly2 <- ts(ts_ds$MeanSubm2,frequency = 12, start = c(2007,2),end = c(2010,11))
plot(tsMonthly2,col = "navy", ylab= "Watt Hours", main = "Laundry Room")


tsMonthly3 <- ts(ts_ds$MeanSubm3,frequency = 12, start = c(2007,2),end = c(2010,11))

plot(tsMonthly3, main="Wh & AC",
     col="navy", 
     ylab="Watt Hours")


### all sub meters in one plot
xx <- ts(ts_ds)
autoplot(xx[,3:7], facets=TRUE) +
  ylab("Electricity Consumption(Watt Hours)")

##tsMonthly1 <- ts(ph_ds$MeanSm3,frequency = 365, start = c(2007,2),end = c(2008,1))
##plot(tsMonthly1,col = "navy",ylab= "Watt Hours",  main = "ac")

ggAcf(tsMonthly3,lag=48)

plot(tsMonthly3)
plot(log(tsMonthly3),main="log")
plot(diff(log(tsMonthly3)),main="diff")
boxplot(tsMonthly3)

monthYearAveCp <- aggregate(fullDs ~ Month + Year,
                            data=fullDs$Sub_metering_3,
                            FUN=mean)

pie(ts_ds$MeanSubm3,
    labels=ts_ds$MeanSubm3,
    main="Month-wise Average for FMCG",
    col=colors()[30:41])
