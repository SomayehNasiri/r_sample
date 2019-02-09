
if (!require(pacman)) install.packages("pacman")
pacman::p_load(arules, arulesViz, tidyverse,RColorBrewer)

library(arules)
library(arulesViz)
require(tidyverse)
install.packages("tidyverse")
require("RColorBrewer")


basketDs <- read.transactions("/home/mohsen/R/data/ElectronidexTransactions2017.csv",
                  format = "basket", sep=",",
                  rm.duplicates=TRUE)

catBasketDs <- read.transactions("/home/mohsen/R/data/ElectronidexTransactions2017categories.csv",
                              format = "basket", sep=";",
                              rm.duplicates=TRUE)



 size(basketDs)
 LIST(basketDs(5))
  itemLabels(basketDs)

itemFrequencyPlot(basketDs,
                  type="absolute",
                  cex.names=0.6,
                  topN=50, # can be changed to the number of interest
                  horiz=TRUE,
                  col='steelblue3',
                  xlab='',
                  main='Item frequency, relative')

itemFrequencyPlot(basketDs,
                  topN=20,
                  cex.names=0.6,
                  col=brewer.pal(8,'Pastel2'),
                  main='Relative Item Frequency Plot',
                  type="relative",
                  ylab="Item Frequency  ")

image(sample(catBasketDs,30))
image(sample(basketDs,50), xlab="Items", ylab="Transactions")
image(basketDs[1:50])

