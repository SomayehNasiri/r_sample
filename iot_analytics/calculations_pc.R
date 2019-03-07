



#### calculating average daily usage

sub_meter1_d <- aggregate(pc_ds$Sub_metering_1,by=list(date_col=pc_ds$Date),FUN=mean)
sub_meter2_d <- aggregate(pc_ds$Sub_metering_2,by=list(date_col=pc_ds$Date),FUN=mean)
sub_meter3_d <- aggregate(pc_ds$Sub_metering_3,by=list(date_col=pc_ds$Date),FUN=mean)
sub_metert_d <- aggregate(pc_ds$total_energy_cons,by=list(date_col=pc_ds$Date),FUN=mean)

daily_consumption_c <- inner_join( sub_meter1_d,sub_meter2_d,
                                   by = "date_col",
                                   suffix = c("dmeter1", "dmeter2"))
daily_consumption_c2 <- inner_join( sub_meter3_d,sub_metert_d,
                                    by = "date_col",
                                    suffix = c("dmeter3", "dmetert"))

daily_consumption <- inner_join(daily_consumption_c,daily_consumption_c2, by = "date_col")

daily_consumption$date_col <- as.Date(daily_consumption$date_col)


#### calculating average yearly usage nd make a data frame
y_sub_meter1 <- aggregate(pc_ds$Sub_metering_1,by=list(year_col=pc_ds$Year),FUN=mean)
y_sub_meter2 <- aggregate(pc_ds$Sub_metering_2,by=list(year_col=pc_ds$Year),FUN=mean)
y_sub_meter3 <- aggregate(pc_ds$Sub_metering_3,by=list(year_col=pc_ds$Year),FUN=mean)
y_sub_metert <- aggregate(pc_ds$Sub_metering_4,by=list(year_col=pc_ds$Year),FUN=mean)

yearly_consumption_c <- inner_join( y_sub_meter1,y_sub_meter2, by = "year_col",suffix = c("meter1", "meter2"))
yearly_consumption_c2 <- inner_join( y_sub_meter3,y_sub_metert, by = "year_col",suffix = c("meter3", "meter4"))
yearly_consumption <- inner_join(yearly_consumption_c,yearly_consumption_c2, by = "year_col")




