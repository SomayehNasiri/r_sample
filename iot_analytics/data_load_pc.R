if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,RColorBrewer,RMySQL,dplyr,lubridate,ggplot2,ggfortify,plotly,ggrepel)


## Create a database connection
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!',
                dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

tbl_2006 <- dbGetQuery(con,"select Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power,global_reactive_power from yr_2006")
tbl_2007 <- dbGetQuery(con,"select Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power,global_reactive_power from yr_2007")
tbl_2008 <- dbGetQuery(con,"select Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power ,global_reactive_power from yr_2008")
tbl_2009 <- dbGetQuery(con,"select Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power,global_reactive_power from yr_2009")
tbl_2010 <- dbGetQuery(con,"select Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power,global_reactive_power from yr_2010")


fullDs <- bind_rows(tbl_2006, tbl_2007, tbl_2008,tbl_2009,tbl_2010)
fullDs <- cbind(fullDs,paste(fullDs$Date,fullDs$Time), stringsAsFactors=FALSE)
colnames(fullDs)[1] <-"DateTime"


fullDs <- fullDs[,c(ncol(fullDs), 1:(ncol(fullDs)-1))]

fullDs$DateTime <- as.POSIXct(fullDs$DateTime, "%Y/%m/%d %h:%m:%s" )

attr(fullDs$DateTime, "tzone") <- "Europe/Paris"

### creating new variables
fullDs$Year <- year(fullDs$DateTime)
fullDs$Quarter <- quarter(fullDs$DateTime)
fullDs$Month <- month(fullDs$DateTime)
fullDs$Week <- week(fullDs$DateTime)
fullDs$Weekday <- weekdays(fullDs$DateTime)
fullDs$Day <- day(fullDs$DateTime)
fullDs$Hour <- hour(fullDs$DateTime)
fullDs$Minute <- minute(fullDs$DateTime)




#add variable for total energy consumption:
fullDs <- mutate(fullDs,
                       total_energy_cons =  Sub_metering_1 + Sub_metering_2 + Sub_metering_3)
#Generate a variable for energy used not measured in the current sub meters (in watt hour)
fullDs <- fullDs %>% mutate(
  Sub_metering_4 = Global_active_power*(1000/60) -
    Sub_metering_1 -
    Sub_metering_2 -
    Sub_metering_3)

####filter2006
pc_ds <- filter(fullDs,fullDs$Year != 2006) 



  