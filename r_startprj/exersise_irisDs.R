install.packages("corrplot")
install.packages("Hmisc")
library(Hmisc)
library(ggplot2)
library(plotly)

#Loading file
exData<- read.csv(file="/home/mohsen/R/data/exercise.csv",header=TRUE,sep = ",")

asData<- read.csv(file="/home/mohsen/R/data/exercise.csv",header=TRUE,sep = ",")
sw<-asData$Sepal.Width
sl<-asData$Sepal.Length
pw<-asData$Petal.Width
pl<-asData$Petal.Length
 ###Calculating Median
sw_m<-median(sw,na.rm = T)
sl_m<-median(sl,na.rm = T)
pw_m<-median(pw,na.rm = T)
pl_m<-median(pl,na.rm = T)

## Replacing NA with median values
asData$Sepal.Width[is.na(asData$Sepal.Width)]<-sw_m
asData$Sepal.Length[is.na(asData$Sepal.Length)]<-sl_m
asData$Petal.Width[is.na(asData$Petal.Width)]<-pw_m
asData$Petal.Length[is.na(asData$Petal.Length)]<-pl_m

## Removing Rows with NA for categorical Data
newAsData<-na.omit(asData)
xData<-newAsData[!complete.cases(newAsData),]

##Rename Column adding new column
colnames(newAsData)[colnames(newAsData)=="Species"] <- "Plants"
newAsData$newcolumn<-newAsData$Petal.Length*newAsData$Petal.Width###must be executed
colnames(newAsData)[colnames(newAsData)=="newcolumn"] <- "Area"

##Removing Outliers Sepal Width
outliers<- boxplot(newAsData$Sepal.Width)$out # actual value for outliers
newAsData[which(newAsData$Sepal.Width %in% outliers),]  ##In which rows outliers are
# Now you can remove the rows containing the outliers
newAsData <- newAsData[-which(newAsData$Sepal.Width %in% outliers),]
# If you check now with boxplot
boxplot(newAsData$Sepal.Width)


#### Removing outliers Sepal Length
outliers<- boxplot(newAsData$Sepal.Length)$out # actual value for outliers
newAsData[which(newAsData$Sepal.Length %in% outliers),] 
newAsData <- newAsData[-which(newAsData$Sepal.Length %in% outliers),]
boxplot(newAsData$Sepal.Length)

##Removing Petal Width outliers
outliers<- boxplot(newAsData$Petal.Width)$out 
newAsData[which(newAsData$Petal.Width %in% outliers),]  
newAsData <- newAsData[-which(newAsData$Petal.Width %in% outliers),]
boxplot(newAsData$Petal.Width)


#### Removing outliers Petal Length
outliers<- boxplot(newAsData$Petal.Length)$out # actual value for outliers
newAsData[which(newAsData$Petal.Length %in% outliers),] 
newAsData <- newAsData[-which(newAsData$Petal.Length %in% outliers),]
boxplot(newAsData$Petal.Length)


### box plot numeric variables
boxplot(newAsData[,1:4],
        ylim=c(0,8),
        horizontl = TRUE,
        las=2,
        notch = TRUE,
        staplelty=1,
        boxwex=0.3,
        whisklty=1,
        outpch=16,
        outcolor="slategray3",
        col="slategray3",
        xlab="Petal Width")

##Histogram
h<-hist(newAsData$Petal.Width,
        breaks = 4,
        #       breaks=seq(0,6,by=1),
        #  breaks=c(0,1,2,3,4,5),
        freq = FALSE,
        col=brewer.pal(4,"Set2"),
        main="Petal width Histogram",
        xlab="Petal Width")
##ggPlot
gDotplot<-ggplot(newAsData, aes(x=Petal.Width,  y=Petal.Length, col=Plants,size=Petal.Width, 
                                shape=Plants,
) )+ geom_point()
gDotplot

# alpha=Petal.Length #transparent the shaapes




?ggplot
gLine_plot<-ggplot(newAsData[newAsData$Plants == "setosa" ,], aes(x=Petal.Width,  y=Petal.Length,col=Plants)) + geom_line(col="red",size=1)
gLine_plot + ggtitle("Petal width va Petal vs Petal Length")+ labs(x="Width",y="Length")+theme_classic()


ggplot(newAsData,aes(Plant~Petal.Length))

#+ scale_x_continuous(breaks = seq(2000,2014,1))
  #+ geom_point(aes( col =newAsData$Plants)) )

##### Correlation MAtrix and Plot
corr <-round(cor(newAsData[,1:4]),2)
corr
cor_sw_sl<- cor.test(newAsData$Sepal.Width,newAsData$Sepal.Length)
cor_sw_sl
 rcorr(as.matrix(newAsData[,1:4]))

round(cor_sw_sl,2)
corrplot(corr, method="pie")
install.packages("caret", dependencies = c("Depends", "Suggests"))



plot(newAsData$Sepal.Length~newAsData$Sepal.Width)
boxplot(newAsData,
       ylim=c(0,8),
        horizontl = TRUE,
        las=2,
        notch = TRUE,
        staplelty=1,
        boxwex=0.3,
        whisklty=1,
        outpch=16,
        outcolor="slategray3",
        #col="slategray3",
       col=brewer.pal(5,"Set2"),
        xlab="Sepal Width",
        main="Boxplot IrisData")

boxplot(exData$Sepal.Length~exData$Sepal.Width,
        col=brewer.pal(4,"Set2"),
        ylim=c(1,10),
        boxwex=0.5)



sp(newAsData$Sepal.Width~newAsData$Sepal.Length | newAsData$Plants)
summary(exData)
list_na <- colnames(exData)[ apply(exData, 2, anyNA) ]
avgsl<-mean(exData$Sepal.Length,na.rm = T)
avgsl

exData$Sepal.Length[is.na(exData$Sepal.Length)]<- avgsl
list_na
sw<-exData$Sepal.Width
avgsw<-mean(sw,na.rm = T)
format(avgsw, digits=2, nsmall=1)
avgsw
exData$Sepal.Width[is.na(exData$Sepal.Width)]<-avgsw
pw<-exData$Petal.Width
pl<-exData$Petal.Length
avgpl<-mean(pl,na.rm = T)
avgpl
format(avgpl, digits = 2, nsmall = 1)
avgpl
colnames(exData)[colnames(exData)=="Species"] <- "Plants"
colnames(exData)[colnames(exData)=="Peta.Area"] <- "Petal.Area"
exData$newcolumn<-pl*pw


round(cor(exData),2)

mean(sl)
mean(sl[sw==3.2])


aggregate(cbind(pw,pl)~exData$Plants,FUN=mean)
boxplot(exData$Sepal.Length~exData$Sepal.Width,
        col=brewer.pal(4,"Set2"),
        ylim=c(1,10),
        boxwex=0.5)
require("RColorBrewer")
average_missing <- apply(exData[,colnames(exData) %in% list_na],
                        2,
                        mean,
                        na.rm =  TRUE)
margin.table(exData,2)
?margin.table
str(exData)
