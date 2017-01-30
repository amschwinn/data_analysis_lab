setwd('C:/Users/amsch/Documents/MLDM/Data Analysis/Final Exam Study Guide/Data')

########
#1) Rush hour at fast food restaurant
#Download the file and read the data
foodDataO <- read.csv("fast_food.csv")
foodData <- foodDataO[,1]

#Part 1
#limit
limit <- c(min(foodData),max(foodData))
#summary
summary(foodData)
#histogram
hist(foodData,xlim = limit)
#mean
foodMean <- mean(foodData)
#standard deviation
foodSD <- sd(foodData)
#Take mean and sd, getting ready to plot
x_g <- seq(limit[1],limit[2],length=200)
y_g <- dnorm(x_g, mean=foodMean, sd=foodSD)

#take the new gaus dist and plot on original graph
par(new=TRUE)
plot(x=x_g,y=y_g,type="l",xlab="",ylab="",col="blue", yaxt="n",xlim=limit)


#Part 2
#Prepare estimation
numCust <- length(foodData)
#start and end times
startTime <- 13.5
endTime <- 18
#get our probability
probability <- (pnorm(endTime, mean=foodMean, sd=foodSD)-pnorm(startTime,mean=foodMean,sd=foodSD))
#use our probability to find the actual number of customers
estimateCusts <- probability * numCust
estimateCusts
#now get our actual customer count
realCusts <- length(foodData[foodData >= startTime & foodData <= endTime])
#No, the data doesn't actually follow a gaussian distribution and our estimate
#assumes that it is,

#Part 3
#Break into subsets
subset1 <- foodData[foodData <= 15]
subset2 <- foodData[foodData >15]

#Part 4
foodMean1 <- mean(subset1)
foodMean2 <- mean(subset2)
foodSD1   <- sd(subset1)
foodSD2   <- sd(subset2)

y_g1 <- dnorm(x_g,mean=foodMean1,sd=foodSD1)
y_g2 <- dnorm(x_g,mean=foodMean2,sd=foodSD2)
par(new=TRUE)
plot(x=x_g,y=y_g1,type="l",xlab="",ylab="",col="red", yaxt="n",xlim=limit)
par(new=TRUE)
plot(x=x_g,y=y_g2,type="l",xlab="",ylab="",col="red", yaxt="n",xlim=limit)

#New estimation
probability1 <- (pnorm(endTime, mean=foodMean1, sd=foodSD1)-pnorm(startTime,mean=foodMean1,sd=foodSD1))
probability2 <- (pnorm(endTime, mean=foodMean2, sd=foodSD2)-pnorm(startTime,mean=foodMean2,sd=foodSD2))
custs1 <- length(subset1)
custs2 <- length(subset2)
newCustEst <- ((probability1*custs1)+(probability2*custs2))
newCustEst
realCusts

######################################
#2) Sold your car
#Download new dataset
carData <- read.csv("light_van_car.csv")
#print summary
summary(carData)
#plot pair representation
pairs(carData)

#Preprocess
carDataLog <- carData
carDataLog[,c(1,2)] <-log(carDataLog[,c(1,2)])
#new pairs
pairs(carDataLog)

#remove the outliers
carDataLog <- carDataLog[-c(12,25),]

#plot the cost as a function of km
plot(y=carDataLog[,"Cost"],x=carDataLog[,"Nb.km"])
plot(carDataLog[,"Cost"]~carDataLog[,"Nb.km"])

#compute simple linear regression
carReg <- lm(Cost~Nb.km, data=carDataLog)

abline(carReg)
Nb.km <- seq(min(carDataLog[,"Nb.km"]),max(carDataLog[,"Nb.km"],length=200))
grid <- data.frame(Nb.km)
CIpred <- predict(carReg, new=grid,interval = "pred",level=.9)
matlines(grid$Nb.km,cbind(CIpred),lty=c(1,2,2),col=2)

carMultReg <- lm(Cost~Nb.km+Years, data=carDataLog)
Nb.km <- c(log(86900))
Years <- c(log(2))
myCar <- data.frame(Nb.km,Years)
ourPrice <-predict(carMultReg,new=myCar)
ourPrice
