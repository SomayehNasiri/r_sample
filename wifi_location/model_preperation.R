

trainingds <- read.csv("data/new_training.csv",
                     sep=",", as.is = TRUE)
validationds <- read.csv("data/new_validation.csv",
                       sep=",", as.is = TRUE)
##I will remove classes that I will not need anymore.

training$RP <- NULL
training$SP <- NULL
training$BU <- NULL
#
validation$RP <- NULL
validation$SP <- NULL
validation$BU <- NULL

##First split the data for each different class.

c <- as.numeric(ncol(training))
Classes <- training[,c(1:c)]
#Classes & Features
LO <- as.data.frame(training[,c(1:(c-3),(c-2))])
LA <- as.data.frame(training[,c(1:(c-3),(c-1))])
FL <- as.data.frame(training[,c(1:(c-3),(c))])
LO2 <- validation[,c(1:(c-3),(c-2))]
LA2 <- validation[,c(1:(c-3),(c-1))]
FL2 <- validation[,c(1:(c-3),(c))]
