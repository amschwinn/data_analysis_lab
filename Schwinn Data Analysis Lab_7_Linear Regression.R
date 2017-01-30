pred#Introduction
#We use regression to estimate the unknown effect of changing one variable over another.
#When runnning a regression we are making two assumptions
#1 there is a linear relationship between two vars (X & Y)
#2 this relationship is addative (ie Y = x1 + x2 + .... xn)
#We use lm(response ~ explanatory1 + explanatory2 + ... + explanatory_p)
setwd("C:/Users/amsch/Documents/MLDM/Data Analysis/Lab")

#2 Simple Linear Regression on Eucalyptus Dataset
#We are trying to estimate the height of the eucalyptus trees based on their circ
euc <- read.table("eucalyptus.txt", header=T)
summary(euc)
plot(ht~circ,data=euc)

#We compute the linear repression
regeuc <- lm(ht~circ, data=euc)
summary(regeuc)
regeuc

#In output, we have the information matrix
#The model ht = B0 + B1 x circ
#ht = 9.037476 + 0.257138 x circ
attributes(regeuc)

#To examine the quality of the model and obsesrvations, we plot the fitted line and observations
#We also drwa the confidence interval of 95%
abline(regeuc, col = "red")
circ=seq(min(euc[,"circ"]),max(euc[,"circ"]),length=100)
grid <- data.frame(circ)
CIpred <- predict(regeuc,new=grid,interval = "pred", level = 0.95)
matlines(grid$circ,cbind(CIpred),lty=c(1,2,2),col=2)

#We will now try a multiple linear regression model ht = B0 + B1 x circ + B2 x sqrt(circ)
multreg <- lm(ht~circ+I(sqrt(circ)),data = euc)
summary(multreg)
plot(ht~circ,data =euc)
circ=seq(min(euc[,"circ"]),max(euc[,"circ"]),length=100)
grid2 <- data.frame(circ)
CIpred2<-predict(multreg,new=grid2,interval = "pred", level=.95)
matlines(grid2$circ, cbind(CIpred2),lty = c(1,2,2),col=2)

#By plotting the model directly, we obtain:
#residuals vs fitted values
#standardized residuals in function of the theoretical quantines (normal Q - Q)
#the sqaure root of the standardized residuals in function of fitted values (scale location)
#the residuals vs the leverage

win.graph(800,600,10)
plot(regeuc)
plot(multreg)

#Simple and Multiple Linear Regression on Ozone Dataset
ozone <- read.table("ozone.txt",header=T)
summary(ozone)

win.graph(800,600,10)
pairs(ozone[1:11])
cor(ozone[1:11])

#Exercise
#With which variable is the maximal ozone concentration(maxO3) most correlated with?
#t12

#plot max03 in function with this variable
plot(ozone$maxO3~ozone$T12)

#compute the simple linear model of max03 in function of this variable
maxReg <- lm(maxO3~T12,data=ozone)
summary(maxReg)

#then trace the line obtained by this model on the graph
abline(maxReg,col="red")

#compute all the numerical variabls (except maxO3)
#and compute the mult lin regression of maxO3 in funcion of these variables
maxMultReg <- lm (maxO3 ~ ., data=ozone)
summary(maxMultReg)

#Plot the results obtained for the simple and multiple linear regressions on ozone dataset
plot(maxReg)
plot(maxMultReg)
