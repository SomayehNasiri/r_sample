

#ds_hourly <- select(fullDs,Date,Hour,Sub_metering_1,Sub_metering_2,
#                    Sub_metering_3,Sub_metering_4,Global_active_power)
ds_hourly  <- select(fullDs,Year,Month,Day,Hour,Sub_metering_1,
                     Sub_metering_2,Sub_metering_3,Sub_metering_4,Global_active_power)

ds_hourly$Date <- as.Date(ds_hourly$Date)
colnames(ds_hourly)[1] <-"dateCol"

ds_hourly <- group_by(ds_hourly,
                      yearCol=ds_hourly$Year,
                      monthCol=ds_hourly$Month,
                      #dayCol=ds_hourly$Day,
                      hourCol=ds_hourly$Hour)

#REDUCING DATA SET SIZE BY USING MEAN INSTEAD OF INDIVIDUAL OBSERVATIONS
ds_hourly <- summarise(ds_hourly, MeanSm3 = mean(Sub_metering_3, na.rm = TRUE),
                         MeanSm2= mean(Sub_metering_2, na.rm = TRUE),
                         MeanSm1= mean(Sub_metering_1, na.rm = TRUE),
                         MeanSm4= mean(Sub_metering_4, na.rm = TRUE),
                         MeanGAP= mean(Global_active_power, na.rm = TRUE))






#hourly_2007 <- subset(ds_hourly,year(ds_hourly$dateCol)==2007 & month(ds_hourly$dateCol)==3 )
#hourly_2010 <-subset(ds_hourly,year(ds_hourly$dateCol)==2010 & month(ds_hourly$dateCol)==3)

hourly_2007 <- filter(ds_hourly, yearCol==2007 & monthCol==3 ) 
hourly_2010 <- filter(ds_hourly, yearCol == 2010 & monthCol==3 )#& Week==3)#& day_2007$Sub_metering_2> 3)
#############Sub meter2 March 2007 and 2010
#xaes <- cbind(ds_hourly,paste(ds_hourly$yearCol,ds_hourly$monthCol,ds_hourly$monthCol,), stringsAsFactors=FALSE)
hourly_df <- data.frame(days=hourly_2007$hourCol,sm2_2007=hourly_2007$MeanSm2,sm2_2010=hourly_2010$MeanSm2)

plot_ly(hourly_df, x = hourly_df$days, y = hourly_df$sm2_2007, name='2007',
        type = 'scatter', mode = 'lines') %>%
  add_trace(y = hourly_df$sm2_2010, name = '2010', mode = 'lines') %>%
  layout(title = "Laundry Room March 2007&2010",
         xaxis = list(title = "Hours of Day"),
         yaxis = list (title = "Average Hourly(watt-hours)"))
############Sub meter 4
hourly_2007_other <- filter(ds_hourly, yearCol==2007 & monthCol==3 ) 
hourly_2010_other <- filter(ds_hourly, yearCol == 2010 & monthCol==3 )

#xaes <- cbind(ds_hourly,paste(ds_hourly$yearCol,ds_hourly$monthCol,ds_hourly$monthCol,), stringsAsFactors=FALSE)
hourly_df_other <- data.frame(days=hourly_2007_other$hourCol,sm4_2007=hourly_2007_other$MeanSm4,sm4_2010=hourly_2010_other$MeanSm4)

plot_ly(hourly_df_other, x = hourly_df_other$days, y = hourly_df_other$sm4_2007, name='2007',
        type = 'scatter', mode = 'lines') %>%
  add_trace(y = hourly_df_other$sm4_2010, name = '2010', mode = 'lines') %>%
  layout(title = "Consumption Other Devices March 2007&2010",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))



sum(hourly_df$sm2_2010/24)



########Calculate daily consumption
sm_ds_daily <- select(fullDs,Date,Sub_metering_1,Sub_metering_2,Sub_metering_3,Sub_metering_4,Global_active_power)
sm_ds_daily$Date <- as.Date(sm_ds_daily$Date)
sm_ds_daily$dateCol <- as.POSIXct(sm_ds_daily$Date, "%Y/%m/%d")
colnames(sm_ds_daily)[1] <-"dateCol"
sm_ds_daily <- group_by(sm_ds_daily,sm_ds_daily$dateCol)

#REDUCING DATA SET SIZE BY USING MEAN INSTEAD OF INDIVIDUAL OBSERVATIONS
sm_ds_daily <- summarise(sm_ds_daily, MeanSm3 = mean(Sub_metering_3, na.rm = TRUE),
                                      MeanSm2= mean(Sub_metering_2, na.rm = TRUE),
                                      MeanSm1= mean(Sub_metering_1, na.rm = TRUE),
                                      MeanSm4= mean(Sub_metering_4, na.rm = TRUE),
                                      MeanGAP= mean(Global_active_power, na.rm = TRUE))



year2007_sm_ds_daily <- filter(sm_ds_daily,(year(sm_ds_daily$dateCol))==2007)
year2008_sm_ds_daily <- filter(sm_ds_daily,(year(sm_ds_daily$dateCol))==2008)
year2009_sm_ds_daily <- filter(sm_ds_daily,(year(sm_ds_daily$dateCol))==2009)
year2010_sm_ds_daily <- filter(sm_ds_daily,(year(sm_ds_daily$dateCol))==2010)

#######calculate one week consumption
week_2007 <- year2007_sm_ds_daily %>% filter(month(year2007_sm_ds_daily$dateCol)==1 & week(year2007_sm_ds_daily$dateCol)==1)
week_2010 <- year2010_sm_ds_daily %>% filter(month(year2010_sm_ds_daily$dateCol)==1 & week(year2010_sm_ds_daily$dateCol)==1)
wdays <- c('1', '2', '3', '4', '5', '6','7')
weekly_df <- data.frame(days=wdays,sm2_2007=week_2007$MeanSm2,sm2_2010=week_2010$MeanSm2)

###########end week

plot_ly(year2007_sm_ds_daily, x = ~year2007_sm_ds_daily$dateCol, y = ~year2007_sm_ds_daily$MeanSm1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~year2007_sm_ds_daily$MeanSm2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~year2007_sm_ds_daily$MeanSm3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~year2007_sm_ds_daily$MeanSm4, name = 'Other Devices', mode = 'lines') %>%
  layout(title = "Average Power Consumption year 2007",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
plot_ly(year2007_sm_ds_daily, x = ~year2007_sm_ds_daily$dateCol, y = ~year2007_sm_ds_daily$MeanGAP, name = 'Global Consumption', type = 'scatter', mode = 'lines') %>%
  layout(title = "Average GAP Power Consumption year 2007",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (Kilo Watt)"))



sm_ds_monthly <- select(fullDs,Year,Month,Sub_metering_1,Sub_metering_2,Sub_metering_3,Sub_metering_4,Global_active_power)


###Summrise data based on Monthh
#COMPRESSING DATA BY GROUPING BY MONTH AND YEAR
sm_ds_monthly <- group_by(sm_ds_monthly, Year, Month) %>% 
  summarise(sm_ds_monthly, MeanGAP = mean(Global_active_power, na.rm = TRUE),
            MeanSubm1 = mean(Sub_metering_1, na.rm = TRUE),
            MeanSubm2 = mean(Sub_metering_2, na.rm = TRUE),
            MeanSubm3 = mean(Sub_metering_3, na.rm = TRUE),
            MeanSubm4 = mean(Sub_metering_4, na.rm = TRUE))

#REDUCING DATA SET SIZE BY USING MEAN INSTEAD OF INDIVIDUAL OBSERVATIONS
sm_ds_monthly <- 
## To remove year 2006
sm_ds_monthly <- sm_ds_monthly[-1,]


colnames(submeter1_mean_month)[1] <- "yearCol"
colnames(submeter1_mean_month)[2] <- "monthCol"


year2007_sm_ds_monthly <- filter(sm_ds_monthly,Year==2007)
year2010_sm_ds_monthly <- filter(sm_ds_monthly,Year==2010)




#####plot first 6 month
x <- c('January', 'February', 'March', 'April', 'May', 'June','July','August','September','October','November','December')

values_6m_2007 <- c(MeanSubm1 = year2007_sm_ds_monthly$MeanSubm1[c(1:12)] ,
                             MeanSubm2 = year2007_sm_ds_monthly$MeanSubm2[c(1:12)],
                             MeanSubm3 = year2007_sm_ds_monthly$MeanSubm3[c(1:12)],
                             MeanSubm4 = year2007_sm_ds_monthly$MeanSubm4[c(1:12)],
                             MeanGAP = year2007_sm_ds_monthly$MeanGAP [c(1:12)])



values_6m_2010 <- c( MeanSubm1 = year2010_sm_ds_monthly$MeanSubm1[c(1:12)] ,
                              MeanSubm2 = year2010_sm_ds_monthly$MeanSubm2[c(1:12)],
                              MeanSubm3 = year2010_sm_ds_monthly$MeanSubm3[c(1:12)],
                              MeanSubm4 = year2010_sm_ds_monthly$MeanSubm4[c(1:12)],
                              MeanGAP = year2010_sm_ds_monthly$MeanGAP [c(1:12)])
######Compare 6 month of 20007 and 2010 GAP
compare_df <- data.frame(day=x,MeanGAP_2007 = year2007_sm_ds_monthly$MeanGAP [c(1:12)],
                                MeanGAP_2010= year2010_sm_ds_monthly$MeanGAP [c(1:12)])
#The default order will be alphabetized unless specified as below:
compare_df$day <- factor(compare_df$day , levels = compare_df[["day"]])

plot_ly(compare_df, x = ~compare_df$day, y = ~compare_df$MeanGAP_2007, name = '2007', type = 'scatter', mode = 'lines') %>%
add_trace(y = ~compare_df$MeanGAP_2010, name = '2010', mode = 'lines') %>%
  layout(title = "Average GAP Power Consumption year 2007&2010",
        xaxis = list(title = "Time"),
         yaxis = list (title = "Average Consumption (Kilo Watt)"))


######Compare all months of 20007 and 2010 SM2

compare_df_sm2 <- data.frame(day=monthList,MeanSubm2_2007 = year2007_sm_ds_monthly$MeanSubm2 [c(1:11)],MeanSubm2_2010= year2010_sm_ds_monthly$MeanSubm2 [c(1:11)])
#The default order will be alphabetized unless specified as below:
compare_df_sm2$day <- factor(compare_df_sm2$day , levels = compare_df_sm2[["day"]])

plot_ly(compare_df_sm2, x = ~compare_df_sm2$day, y = ~compare_df_sm2$MeanSubm2_2007, name = '2007', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~compare_df_sm2$MeanSubm2_2010, name = '2010', mode = 'lines') %>%
  layout(title = "Laundry Room 2007&2010",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Average Monthly(watt-hour)"))



######Compare all months of 20007 and 2010 SM4
monthList <- c('January', 'February', 'March', 'April', 'May', 'June','July','August','September','October','November')
compare_df_sm4 <- data.frame(day=monthList,MeanSubm4_2007 = year2007_sm_ds_monthly$MeanSubm4 [c(1:11)],MeanSubm4_2010= year2010_sm_ds_monthly$MeanSubm4 [c(1:11)])
#The default order will be alphabetized unless specified as below:
compare_df_sm4$day <- factor(compare_df_sm4$day , levels = compare_df_sm4[["day"]])

plot_ly(compare_df_sm4, x = ~compare_df_sm4$day, y = ~compare_df_sm4$MeanSubm4_2007, name = '2007', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~compare_df_sm4$MeanSubm4_2010, name = '2010', mode = 'lines') %>%
  layout(title = "Other Devices 2007&2010",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Average Monthly(watt-hour)"))



############# Compare whole years
year_vector_2007 <- c(seq.Date(from = date('2007-01-01'), to = date('2007-12-31'), by = "month"))
Yearly_values <- data.frame( day = year_vector_2007, MeanSubm1 = year2007_sm_ds_monthly$MeanSubm1[c(1:12)] ,
                             MeanSubm2 = year2007_sm_ds_monthly$MeanSubm2[c(1:12)],
                             MeanSubm3 = year2007_sm_ds_monthly$MeanSubm3[c(1:12)],
                             MeanSubm4 = year2007_sm_ds_monthly$MeanSubm4[c(1:12)],
                             MeanGAP = year2007_sm_ds_monthly$MeanGAP [c(1:12)])



#####Monthly consumption for all years 

year_vector <- c(seq.Date(from = date('2007-01-01'), to = date('2010-11-30'), by = "month"))
Yearly_values <- data.frame( day = year_vector, MeanSubm1 = sm_ds_monthly$MeanSubm1[c(1:47)] ,
                             MeanSubm2 = sm_ds_monthly$MeanSubm2[c(1:47)],
                             MeanSubm3 = sm_ds_monthly$MeanSubm3[c(1:47)],
                             MeanSubm4 = sm_ds_monthly$MeanSubm4[c(1:47)],
                             MeanGAP = sm_ds_monthly$MeanGAP [c(1:47)])


plot_ly(Yearly_values, x = ~Yearly_values$day, y = ~Yearly_values$MeanSubm1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Yearly_values$MeanSubm2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~Yearly_values$MeanSubm3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~Yearly_values$MeanSubm4, name = 'Other Devices', mode = 'lines') %>%
  layout(title = "Average Power Consumption year 2007",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(Yearly_values, x = ~Yearly_values$day, y = ~Yearly_values$MeanGAP, name = 'Global Consumption', type = 'scatter', mode = 'lines') %>%
  layout(title = "Average GAP Power Consumption 2007-2010",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (Kilo Watt)"))
############# end alll years

#####Monthly consumption for year 2007
plot_ly(Yearly_values, x = ~Yearly_values$day, y = ~Yearly_values$MeanSubm1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Yearly_values$MeanSubm2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~Yearly_values$MeanSubm3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~Yearly_values$MeanSubm4, name = 'Other Devices', mode = 'lines') %>%
  layout(title = "Average Power Consumption year 2007",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(year2007_sm_ds_monthly, x = ~year2007_sm_ds_monthly$Month, y = ~year2007_sm_ds_monthly$MeanGAP, name = 'Global Consumption', type = 'scatter', mode = 'lines') %>%
  layout(title = "Average GAP Power Consumption year 2007",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (Kilo Watt)"))


plot_ly(year2007_sm_ds_daily, x = ~year2007_sm_ds_daily$dateCol, y = ~year2007_sm_ds_daily$MeanGAP, name = 'Global Consumption', type = 'scatter', mode = 'lines') %>%
  layout(title = "Average GAP Power Consumption year 2007",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (Kilo Watt)"))


########## A plot to have predicted and original values at the same time
yearly_consumption

de <- data.frame(2011, 0.9605941, 0.9305483, 6.87044, 9.032619 )
names(de) <- names(yearly_consumption)
yearly_consumption <- rbind(yearly_consumption, de)
yearly_consumption$year_col <- as.factor(yearly_consumption$year_col)

plot_ly(yearly_consumption, x = ~yearly_consumption$year_col, y = ~yearly_consumption$xmeter1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~yearly_consumption$xmeter2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~yearly_consumption$xmeter3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~yearly_consumption$xmeter4, name = 'Other Devices', mode = 'lines') %>%
  layout(title = "Power Consumption & Prediction",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

###########
sm2_ds <- select(fullDs,Year,Sub_metering_2)


###Summrise data based on Monthh
#COMPRESSING DATA BY GROUPING BY MONTH AND YEAR
sm2_ds <- group_by(sm2_ds, Year)

#REDUCING DATA SET SIZE BY USING MEAN INSTEAD OF INDIVIDUAL OBSERVATIONS
sm2_ds <- summarise(sm2_ds,
                           SumSubm2 = sum(Sub_metering_2, na.rm = TRUE))

df_month_2008 <- sm2_ds %>% filter( Year == 2008)
df_week_2008 <- sm2_ds %>% filter( Year == 2008)
df_day_2008 <- sm2_ds %>% filter( Year == 2008)
df_year_2008 <- sm2_ds %>% filter( Year == 2008)

df_year_untilnow <- summarise(sm2_ds,sumall=sum(sm2_ds$SumSubm2))


p = ggplot(year2007_sm_ds_daily, aes(x=year2007_sm_ds_daily$dateCol, y=year2007_sm_ds_daily$MeanSm3))# +
  geom_segment( aes(x=year2007_sm_ds_daily$dateCol, xend=year2007_sm_ds_daily$dateCol, y=0, yend=year2007_sm_ds_daily$MeanSm3 ), color=ifelse(data$x %in% c("A","D"), "orange", "grey"), size=ifelse(data$x %in% c("A","D"), 1.3, 0.7) ) +
  geom_point( color=ifelse(data$x %in% c("A","D"), "orange", "grey"), size=ifelse(data$x %in% c("A","D"), 5, 2) ) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="none",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  xlab("") +
  ylab("Value of Y")
p
#preprocess yearly trend values
year_vector_2007 <- c(seq.Date(from = date('2007-01-01'), to = date('2007-12-31'), by = "day"))
Yearly_values <- data.frame(values = forecast_GAP$yearly[c(1:365)], 
                            day = year_vector_2007)

#plot yearly trend
ggplot(Yearly_values) +
  geom_hline(yintercept = 0, size = 1, colour="#333333", linetype="dashed") +
  geom_path(aes(x = day, y = values), colour = "#FAAB18", size = 1.2, group = 1) + 
  geom_hline(yintercept = -.6, size = 1, colour="#333333") +
  bbc_style() +
  labs(title="Yearly Trend",
       subtitle = "Relative Electricity Usage In A Year")


