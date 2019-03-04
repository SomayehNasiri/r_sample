

pf_ds <- bind_rows(tbl_2007,tbl_2010)

pf_ds <- cbind(pf_ds,paste(pf_ds$Date,pf_ds$Time), stringsAsFactors=FALSE)
colnames(pf_ds)[8] <-"DateTime"


pf_ds <- pf_ds[,c(ncol(pf_ds), 1:(ncol(pf_ds)-1))]

pf_ds$DateTime <- as.POSIXct(pf_ds$DateTime, "%Y/%m/%d %H:%M:%S")

attr(pf_ds$DateTime, "tzone") <- "Europe/Paris"





pf_ds$ApparentPower <- sqrt( ((pf_ds$Global_active_power)^2) + ( (pf_ds$global_reactive_power)^2))
pf_ds$PowerFactor <- (pf_ds$Global_active_power/pf_ds$ApparentPower)
plotly(pf_ds, x=pf_ds$DateTime,y= pf_ds$global_reactive_power)

pf_ds_daily <- select(pf_ds,Date,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power,global_reactive_power,ApparentPower,PowerFactor)
pf_ds_daily$Date <- as.Date(pf_ds_daily$Date)
#sm_ds_daily$dateCol <- as.POSIXct(sm_ds_daily$Date, "%Y/%m/%d")

colnames(pf_ds_daily)[1] <-"dateCol"
pf_ds_daily <- group_by(pf_ds_daily,pf_ds_daily$dateCol)

#REDUCING DATA SET SIZE BY USING MEAN INSTEAD OF INDIVIDUAL OBSERVATIONS
pf_ds_daily <- summarise(pf_ds_daily, 
                         MeanSm1= mean(Sub_metering_1, na.rm = TRUE),
                          MeanSm2= mean(Sub_metering_2, na.rm = TRUE),
                        
                         MeanSm3 = mean(Sub_metering_3, na.rm = TRUE),
                        
                         MeanGAP= mean(Global_active_power, na.rm = TRUE),
                         MeanGRP= mean(global_reactive_power, na.rm = TRUE),
                         MeanAP= mean(ApparentPower, na.rm = TRUE),
                         MeanPF= mean(PowerFactor, na.rm = TRUE))
colnames(pf_ds_daily)[1] <-"dateCol"
plot_ly(pf_ds_daily, x = ~pf_ds_daily$dateCol, y = ~pf_ds_daily$MeanGAP, name = 'GAP', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~pf_ds_daily$MeanGRP, name = 'GRP', mode = 'lines') %>%
  layout(title = "GAP GRP 2007,2010",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))



sm_ds_monthly_g <- select(pf_ds,Year,Month,Sub_metering_1,Sub_metering_2,Sub_metering_3,Sub_metering_4,Global_active_power,global_reactive_power,ApparentPower,PowerFactor)


###Summrise data based on Monthh
#COMPRESSING DATA BY GROUPING BY MONTH AND YEAR
sm_ds_monthly <- group_by(sm_ds_monthly, Year, Month)

#REDUCING DATA SET SIZE BY USING MEAN INSTEAD OF INDIVIDUAL OBSERVATIONS
sm_ds_monthly <- summarise(sm_ds_monthly, MeanGAP = mean(Global_active_power, na.rm = TRUE),
                           MeanSubm1 = mean(Sub_metering_1, na.rm = TRUE),
                           MeanSubm2 = mean(Sub_metering_2, na.rm = TRUE),
                           MeanSubm3 = mean(Sub_metering_3, na.rm = TRUE),
                           MeanSubm4 = mean(Sub_metering_4, na.rm = TRUE))
## To remove year 2006
sm_ds_monthly <- sm_ds_monthly[-1,]

