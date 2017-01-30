#install.packages("XLConnect")
library("XLConnect")

planets <- readWorksheet(loadWorkbook("C:/Users/amsch/Documents/MLDM/Data Analysis/Lab/planets.xlsx"), sheet=1)
planets
names <- planets[,1]
rownames(planets) <- names
planets[,1] <- NULL
planets

#Pairwise Representation
#The dataset has now 3 numerical variables. We can represent the data 2 by 2 by using the pairwise
#representation with the function pairs().
pairs(planets)

#It is difficult to make a relevant conclusion just by seeing this plot. With the diameter and distance
#variables, some points are concentrated on an unique set (a cluster).
#This is due to the astronomical scale. We can fix this problem by taking into account the log transform of
#each variable.
planets.log <- log(planets)
colnames(planets.log) <- paste("log(",colnames(planets),")",sep="")
pairs(planets.log)

#This new representation is better than the previous one, but we have many plots to study (if we have d
#variables, we can have d × (d ??? 1) or at least d×(d???1)/2 plots to see), and we don't have the information of the
#name of the planets.

#2D Representation and Plot Size
#In this context, we have one dimension representing the diameter of the planets (the 2nd var). We
#can use this information for changing the size of teh plot and represent the data with the other two vars
#for the X and Y axis.
#We will plot our graph in a new window, e.g. with the size of 800x600 pixels
#Create the plot wihtout plotting the points
win.graph(800,600,10)

plot(x=planets.log[,1], y=planets.log[,3], cex=(round(planets.log[,2])), 
     xlab = colnames(planets.log[1]), ylab = colnames(planets.log[3]))

#write text to the position of each point
text(x=planets.log[,1], y=planets.log[,3],
     labels=names, cex=.8, col="blue")

#3D Representation
#install.packages("rgl")
library("rgl")

#In R, we can easily represent in 3D with the rgl package.
plot3d(x=planets.log[,1],
       y=planets.log[,2],
       z=planets.log[,3],
       xlab = colnames(planets.log[1]),
       ylab = colnames(planets.log[2]),
       zlab = colnames(planets.log[3]))
text3d(x=planets.log[,1],
       y=planets.log[,2],
       z=planets.log[,3],
       text=names,
       cex=0.8,
       col="blue")

#Representation with Dimensionality Reduction
planets.pca <- princomp(planets.log)
planets.pca
win.graph(800,600,10)
biplot(planets.pca)

#PCA on a 2D Dataset, Step by Step
#Compue the PCA for X = {(1, 2),(3, 3),(3, 5),(5, 4),(5, 6),(6, 5),(8, 7),(9, 8)}.
#Plot the data in the original 2d space
x <- c(1,3,3,5,5,6,8,9)
y <- c(2,3,5,4,6,5,7,8)
mydataset <- cbind(x,y)
mydataset
plot(mydataset[,"x"],mydataset[,"y"])

#Note that for the plot function the option asp gives the aspect ration y.x. 
#If asp is a finite positive value then the window is set up so that one data unit in the x direction
#is equal in length to asp x one data unit in the y direction. 
#The special case asp==1 productes plots where the distance between points are represented accurately on the screen.
plot(mydataset[,"x"],mydataset[,"y"],asp=1)

#Compute the covariance matrix ?? from the zero mean values (x ??? x)
n <- nrow(mydataset)
d <- ncol(mydataset)
xbar = mean(x)
ybar = mean(y)
newX = x - xbar
newY = y - ybar

#Covariance without correction
covariance <- 0
for (i in 1:n) {
  covariance <- covariance + newX[i] * newY[i]
}

covariance <- covariance/(n)
covariance

#Covariance with correction
covariance <- 0
for(i in 1:n){
  covariance <- covariance + newX[i] * newY[i]
}
covariance <- covariance / (n-1)
covariance

#OR we can use the function cov
mynewdataset <- data.frame(newX,newY)
covariance <- cov(mynewdataset)
covariance

#Solve the characteristic equation to get the eigenvalues and deduce the
#eigenvectors, then compute the  2 first componants.
E <- eigen(covariance)
E$vectors
E$values
Component1 <- (E$vectors[1]/E$vectors[2])*newX
Component1
Component2 <- (E$vectors[3]/E$vectors[4])*newY
Component2

#Plot the scatterplot of the dataset, then the first 2 components
#They will intersect perpendicularly (on an orthogonal graph)
win.graph(800,600,10)
plot(mynewdataset$newX, mynewdataset$newY,
     col="black", xlim=c(-6,6), ylim=c(-6,6), asp =1)
par(new=TRUE)
plot(mynewdataset$newX, Component1, xlab="", ylab="",
    col="blue", type="l", xlim=c(-6,6), ylim=c(-6,6), asp =1)
par(new=TRUE)
plot(mynewdataset$newY, Component2, xlab="", ylab="",
     col="red", type="l", xlim=c(-6,6), ylim=c(-6,6), asp =1)

#Plot the biplot of the PCA obtained with teh function princomp
mynewdataset.pca <- princomp(mynewdataset)
win.graph(800,600,10)
biplot(mynewdataset.pca)

#PCA on a High-Dimensional Dataset
#Load the excel dataset, label each row with the col of name, then remove unnecessary col
rm(list=ls())
students <- readWorksheet(loadWorkbook("C:/Users/amsch/Documents/MLDM/Data Analysis/Lab/students_2015.xlsx"), sheet=1)
students
#We can also use the new dataset of this year's class
#students <- readWorksheet(loadWorkbook("C:/Users/amsch/Documents/MLDM/Data Analysis/Lab/MLDM_students_2016.xlsx"), sheet=1)
names <- students[,1]
rownames(students) <- names
students[,1] <- NULL
students

#Descriptive Statistics
#We can print some useful info about the dataset
#Summary
summary(students)
#Number of observations
n <- nrow(students)
print(n)
#Object Properties
attributes(students)

#Pairwise Representation, 2D-Study
#By plotting the scatterplot matrices with the function pairs(), we can study the relation between the vars two by two
students$LatBP_NS <- as.numeric(students$LatBP_NS)
students$Long_BP_EW <- as.numeric(students$Long_BP_EW)
pairs(students)
cor(students)

#Age and NbDays are have very strong correlation becuase Age is NbDays/365.25
#Lets plot that in a new window
win.graph(800,600,10)
plot(students$NbDays, students$Age,
     xlim=c(7700,12000), ylim=c(20,32))
par(new=TRUE)
x <- seq(7700,12000, length=2000)
y <- floor(x/365.25)
plot(x,y, col = "red", type="l", xlab="", ylab="",
     xlim=c(7700,12000), ylim=c(20,32))

#PCA with princomp function
#We will compute the PCA with the parameters cor=TRUE for indicating that the calc should
#use the correlation and scores = TRUE for indicating that the score on each princ comp needs to be calc
pca.students <- princomp(students, cor = TRUE, scores =TRUE)
pca.students
summary(pca.students)
attributes(pca.students)

#Eigenvalues (3.5)
#We can now calculate and print the variances associated with the axis and plotting the scree plot
eigenvalues <- pca.students$sdev^2
eigenvalues

plot(1:4,eigenvalues, type="b", ylab="Eigenvalues",
     xlab = "Components", main="Scree_Plot")

#We will pring the table of the eigeigenvalues for each component with a confidence interval ?? = 95%
#Confidence interval of the eigenvalues at 95%
#Hint:
# pnorm ( 1 . 9 6 , mean=0 , s d =1) ??? pnorm ( ???1. 9 6 , mean=0 , s d =1) = 0 . 9 5
val.low <- eigenvalues*exp(-1.96 * sqrt(2.0/(n-1)))
val.high <- eigenvalues*exp(+1.96 * sqrt(2.0/(n-1)))
#Table
table <- cbind(val.low,eigenvalues,val.high)
colnames(table) <- c("Lower_Bound", "EigenValues", "Higher_Bound")
print(table, digits = 3)

#Graphs (3.6)
#3.6.1
#Coordinates of the variables and factorial axes
#Frist, we have to compute some interesting values (correlations between variables and all the componenets, square value of the correlations
#and cumulative square of the correlations)
#Variables-Axes Correlation
c1 <- pca.students$loadings[,1]*pca.students$sdev[1]
c2 <- pca.students$loadings[,2]*pca.students$sdev[2]

#Correlation
correlation <- cbind(c1,c2)
print(correlation,digits=2)

#Square value of the correlation
print(correlation^2,digits=2)

#Cumulatie square of the correlation
print(t(apply(correlation^2,1,cumsum)),digits=2)

#3.6.2
#Correlation Circle Plot
#We can now plot the correlation circle
win.graph(800,600,10)
plot(c1,c2,xlim=c(-1,1),ylim=c(-1,1),type="n")
abline(h=0,v=0)
text(c1,c2,labels=colnames(students),cex=1,col="red")
symbols(0,0,circles=1,inches=FALSE,add=TRUE)

#3.6.3
#Representation of the Students in the first factorial plane
#We can use the two first componenets for plotting the students
win.graph(800,600,10)
plot(pca.students$scores[,1],
     pca.students$scores[,2],
     type="n", xlab = "Comp.1", ylab = "Comp.2")
abline(h=0,v=0)
text(pca.students$scores[,1],
     pca.students$scores[,2],
     labels = rownames(students), cex=.75)
#For simultaneous representation of the variables and observations, we can use the biplot function
biplot(pca.students, cex=.75)

#3.6.4
#Representation of the students in the first factorial cube
#with the first 2 components, we represent only 70% of the inertia of the students dataset. 
#with the 3 first componenets, what is the cumulative proportion represented?
summary(pca.students)
#93%

library("rgl")
plot3d(x = pca.students$scores[,1],
       y=pca.students$scores[,2],
       z=pca.students$scores[,3],
       xlab="Comp.1",
       ylab="Comp.2",
       zlab="Comp.3")
text3d(x=pca.students$scores[,1],
       y=pca.students$scores[,2],
       z=pca.students$scores[,3],
       xlab="",
       ylab="",
       zlab="",
       text=names,
       cex=.8,
       col="blue")
#In conclusion, is it relevant to represent the data in a 3D-space?
#In this instace, the data is very revelant with 3 dimensions since 3 componenets represent over 90% of the cumulative proportion
#However, in graph form there is too much going on in a chartf to visualize it well