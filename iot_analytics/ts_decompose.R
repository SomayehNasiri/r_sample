
###Decomposed graphs for submeter1
components_tsMonthly1 <- decompose(tsMonthly1)
autoplot(components_tsMonthly1, facets=TRUE) +
  ylab("Electricity Consumption(Watt Hours)") +
  ggtitle("Kitchen") 
summary(components_tsMonthly1)


###Decomposed graphs for submeter2
components_tsMonthly2 <- decompose(tsMonthly2)

autoplot(components_tsMonthly2, facets=TRUE) +
  ylab("Electricity Consumption(Watt Hours)")+
  ggtitle("Laundry Room") 
summary(components_tsMontly2)

###Decomposed graphs for submeter3
components_tsMonthly3 <- decompose(tsMonthly3)
plot(components_tsMonthly3, facets=TRUE)
autoplot(components_tsMonthly3, facets=TRUE) +
  ylab("Electricity Consumption(Watt Hours)") +
  ggtitle("Water Heater & AC") 

summary(components_tsMonthly3)
### Holt winter


tsMonthly3justed <- tsMonthly3 - components_tsMonthly3$seasonal
autoplot(tsMonthly3justed)
plot(decompose(tsMonthly3justed))

## Holt Winters Exponential Smoothing & Plot
tsSM3_HW <- HoltWinters(tsMonthly3justed, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW, ylim = c(0, 25))

## HoltWinters forecast & plot
tsSM3_HWfor <- forecast(tsSM3_HW, h=25)
plot(tsSM3_HWfor, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

## Forecast HoltWinters with diminished confidence levels
tsSM3_HWforC <- forecast(tsSM3_HW, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HWforC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))
