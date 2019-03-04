library(xts)
library(forecast)

y <- ts(yearly_consumption$xtotal_meter, start=2003)#, frequency=12)
yy <- ts(yearly_consumption$xmeter1, start=2003)#, frequency=12)
y <- ts(yearly_consumption$xmeter2, start=2003)#, frequency=12)
y <- ts(yearly_consumption$xmeter3, start=2003)#, frequency=12)

x <- xts(x=yearly_consumption)
pc_ds$DateTime



daily_xts <- as.xts(daily_ts)

autoplot(daily_xts) +
  ggtitle("") +
  xlab("Year") +
  ylab("Thousands")

# myts2 <- window(daily_ts, start=c(2008, 6), end=c(2008, 10)) 
daily_ts1 <- ts(sub_meter1_d$x, start=c(2007,01,01),end = c(2009,12,31), frequency=12)
ggseasonplot(daily_ts1, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Power (watt-hours)") +
  ggtitle("Monthly:Average Energy Consumption in Kitchen(2007,2008,2009)")


daily_ts2 <- ts(sub_meter2_d$x, start=c(2007,01,01),end = c(2009,12,31), frequency=12)
ggseasonplot(daily_ts2, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Power (watt-hours)") +
  ggtitle("Monthly:Average Energy Consumption in Laundry Room(2007,2008,2009)")



daily_ts3 <- ts(sub_meter3_d$x, start=c(2007,01,01),end = c(2009,12,31), frequency=12)
ggseasonplot(daily_ts3, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Power (watt-hours)") +
  ggtitle("Monthly:Average  water Heater and AC Energy Consumption(2007,2008,2009)")


ggsubseriesplot (daily_ts3, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("watt") +
  ggtitle("Monthly plot: :Average  water Heater and AC Energy Consumption(2007,2008,2009)")



plot(daily_ts1)
abline(reg = lm(daily_ts1~time(daily_ts1)))
boxplot(daily_ts1~cycle(daily_ts1))
sub_meter1_d$date_col <- as.Date(sub_meter1_d$date_col)
kk <- filter(sub_meter1_d,year(sub_meter1_d$date_col)==2007 & month(sub_meter1_d$date_col)==1)
df <- submeter1_sum_month[,2:4]

ss <- ts(ts_ds,start=c(2007,01,01),end = c(2009,12,31), frequency=12)
plot(ss)
