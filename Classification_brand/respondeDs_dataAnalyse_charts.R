install.packages("devtools")
library(devtools)
library(ggplot2)

require("RColorBrewer")
respDs <- read.csv(file="/home/mohsen/R/data/CompleteResponses.csv",header=TRUE,sep = ",")
predDs <- read.csv(file="/home/mohsen/R/data/SurveyIncomplete.csv",header=TRUE,sep = ",")

respDs$brand <- as.factor(respDs$brand)
predictionDs$brand <- as.factor(predictionDs$brand)

### make age and salart catogirical
catAge <- cut(respDs$age,breaks = c(19,40,60,81),labels=c( "20-40","40-60","60+"))
catSal <- cut(respDs$salary,breaks = c(19000,60000,100000,160000),labels=c("Low","Medium","High"))
##catAge <- cut(respDs$age,breaks = c(-Inf,20,40,60,80,Inf),labels=c("0-20", "20-40","40-60","60-80","80+"))

catAge_pre <- cut(predDs$age,breaks = c(19,40,60,81),labels=c("20-40","40-60","60+"))
catSal_pre <- cut(predDs$salary,breaks = c(19000,60000,100000,160000),labels=c("Low","Medium","High"))

##Box plot current data with brand preference
bp <- ggplot(respDs, aes(x=catAge,y=salary))+ geom_boxplot(aes(fill = brand)) ###+geom_point()
bp+ theme(
  legend.background = element_rect(color = "steelblue",linetype = "solid") + 
                      scale_fill_discrete(name = "Brand", 
                      labels=c("Acer","Sony")))

######### Boxplot predicted data for predicted brands values
bp_pre <- ggplot(predictionDs, aes(x=catAge_pre,y=salary))+ geom_boxplot(aes(fill = brand)) ###+geom_point()
bp+ theme(legend.background = element_rect(color = "steelblue", linetype = "solid")+
            scale_color_manual( labels=c("Acer","Sony"), values = c(0,1)))



grid.arrange(bp, bp_pre, ncol=2)
#########histogram for salary and age current and predicted answers
df1 <- data.frame(catAge )
df2 <- data.frame(catAge_pre)
df <- rbind(df1, df)

aop <- ggplot(respDs, aes(catAge)) + geom_histogram(  binwidth = 0.2,
                                                      fill="lightgreen",
                                                      xlab="Age",
                                                      main="Current Customers",
                                                      stat = "count",
                                                       brewerPalette="Blues")
aop
app <- ggplot(predDs, aes(catAge_pre)) + geom_histogram(  binwidth = 0.5,
                                                          xlab="Age",
                                                          fill="lightblue",
                                                          stat = "count",
                                                          main="New Customers",
                                                          brewerPalette="Blues")
app
grid.arrange(aop, app, ncol=2)

###########
hbr <- ggplot(respDs, aes(brand)) + geom_histogram(  binwidth = 0.5,
                                                    xlab="Age",
                                                    fill="lightblue",
                                                    stat = "count",
                                                    main="New Customers",
                                                    brewerPalette="red")
hbr
hbp <- ggplot(predictionDs, aes(brand)) + geom_histogram(  binwidth = 0.5,
                                                   xlab="Age",
                                                   fill="lightpink",
                                                   stat = "count",
                                                   main="New Customers",
                                                   brewerPalette="red")
hbp


grid.arrange(hbr, hbp, ncol=2)
##### bar plot salary and predicted salary
aop <- ggplot(respDs, aes(catSal)) + geom_histogram(  binwidth = 0.2,
                                                      fill="lightpink",
                                                      xlab="Age",
                                                      main="Current Customers",
                                                      stat = "count",
                                                      brewerPalette="Blues")
aop
app <- ggplot(predDs, aes(catSal_pre)) + geom_histogram(  binwidth = 0.5,
                                                          xlab="Age",
                                                          fill="yellow",
                                                          stat = "count",
                                                          main="New Customers",
                                                          brewerPalette="Blues")
app
grid.arrange(aop, app, ncol=2)

###Joan scatter for mixing to charts in same DS
ggplot(iris1ds)+
  geom_point(aes(x = Sepal.Length, y = Sepal.Width), color = red) +
  geom_point(aes(x = Petal.Length, y = Petal.Width), color = blue)

mydata <- data.frame(myGroup = c(catAge, catAge_pre), myX = c(1,1))

qplot(data = mydata, 
      x = myX, 
      facets = ~myGroup)

ggplot(data = mydata) + 
  geom_bar(aes(myX)) + 
  facet_wrap(~myGroup)



ggplot(respDs, aes(age)) + geom_histogram(binwidth = 0.5)

ggplot(respDs, aes(x=age ,y=salary)) + geom_bar()

##  geom_line(col="red",size=1)

hist(predDs[-7,-5,]$age,
      breaks = 4,
      #       breaks=seq(0,6,by=1),
      #  breaks=c(0,1,2,3,4,5),
      freq = FALSE,
      col=brewer.pal(4,"Set2"),
      main="Petal width Histogram",
      xlab="Age")
 
 hist(predDs[-7,-5,]$salary,
      breaks = 4,
      #       breaks=seq(0,6,by=1),
      #  breaks=c(0,1,2,3,4,5),
      freq = FALSE,
      col=brewer.pal(4,"Set2"),
      main="Petal width Histogram",
      xlab="Age")
 
 
 ggplot2.histogram(data=respDs, xName='weight',
                   fill="white", color="black",
                   addMeanLine=TRUE, meanLineColor="red",
                   meanLineType="dashed", meanLineSize=1)
 