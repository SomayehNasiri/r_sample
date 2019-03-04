
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,RColorBrewer,RMySQL,dplyr,lubridate,ggplot2,ggfortify,plotly,reshape2,CGPfunctions)


mydata <- structure(list( Year = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L), 
                                         .Label = c("2007", "2010"), 
                                         class = "factor"), 
                        Submeters = structure(c( 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L), 
                                          .Label = c("Kitchen", "Laundry Room", "Water Heater & AC", "Other Devices"), 
                                          class = "factor"), 
                        Average = c(1.23,1.63,5.79,9.95,0.98,1.10,7.24,8.35)), 
                  class = "data.frame", 
                  row.names = c(NA, -8L))


ggplot(data = mydata, aes(x = Year, y = Average, group = Submeters)) +
  geom_line(aes(color = Submeters), size = 2) +
  geom_point(aes(color = Submeters), size = 4) +
  #  Labelling as desiredplot
  labs(
    title = "Electricity Consumption Trend 2007-2010",
    subtitle = "",
    caption = ""
  ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())




monthly_ds <- select(fullDs,Year,Month,Sub_metering_1,Sub_metering_2,Sub_metering_3,Sub_metering_4)
###Summrise data based on Monthh
#COMPRESSING DATA BY GROUPING BY MONTH AND YEAR
monthly_ds <- group_by(monthly_ds, Year, Month)

#REDUCING DATA SET SIZE BY USING MEAN INSTEAD OF INDIVIDUAL OBSERVATIONS
monthly_ds <- summarise(monthly_ds, 
                   Kitchen = sum(Sub_metering_1, na.rm = TRUE),
                   Laundry_Room = sum(Sub_metering_2, na.rm = TRUE),
                    WH_AC= sum(Sub_metering_3, na.rm = TRUE),
                   OtherDevices = sum(Sub_metering_4, na.rm = TRUE))

monthly_ds <- arrange(monthly_ds, Year, Month)
monthly_ds_2008 <- monthly_ds %>% filter(Year==2008)
### Delete Year column
monthly_ds_2008 <- monthly_ds_2008[, -1]

monthly_ds_2008$Month<- month(monthly_ds_2008$Month, label = TRUE, abbr = TRUE, locale = Sys.getlocale("LC_TIME"))

monthly_ds_2008$monthName <- as.factor(monthly_ds_2008$Month )
################### gather function
DailyMeanGathered <- monthly_ds_2008 %>% 
  gather(key = 'sub', value = 'value', 
         OtherDevices,WH_AC,Laundry_Room,Kitchen) %>% 
  rename(ds = Month,
         y = value) 
#################
DF1 <- melt(monthly_ds_2008, id.var="Month")

ggplot(DF1) + 
  geom_bar(aes(x =monthName, y = value, fill = variable), position = "fill", stat = 'identity')+
  labs(title = "Electricity Consumption SM 2008", x="Month",y="Percentage")
 



month(1, label = TRUE, abbr = TRUE, locale = Sys.getlocale("LC_TIME"))
month(x) <- value


DF <- read.table(text="Rank F1     F2     F3
1    500    250    50
2    400    100    30
3    300    155    100
4    200    90     10", header=TRUE)
