setwd("C:/Users/amsch/Documents/MLDM/Data Analysis/Final Exam Study Guide/Data")

#Question 1) Baking as a chef
#Total amounts of products
totals <- matrix(c(7750, 7630, 31500,333,27600), ncol=1) 
#Matrix of all the recipes
recipes <- matrix(c(125,125,125,2,0,150,70,400,3,80,50,20,250,4,500
                   ,0,60,0,5,600,120,120,1000,4,300),nrow=5) 

#To solve the problem we model it as X = A^-1 *b 
#where A is the recipes matrix & b is our totals  

#First calculate the inverse of the recipes matrix
Inv <- solve (recipes)  
#Compute Inv of A * b to get X
Solution <- Inv%%totals
#We found the solution to the system of equations
#Now we combine that with our predifined food constraints 
#to find the number of people we will serve
Number_people <- Solution[1,1]*6+Solution[2,1]*8+
  Solution[3,1]*10+Solution[4,1]*5+Solution[5,1]*6
Number_people
# We know that we can serve 600 hundred people




#################################################
#Question 2) OffShore Fishing
#2.1)
#Read the file
fishData <- read.csv("fishes_MLDM.csv")
#Plot the pairwise representation
pairs(fishData)
#How many clusters?
#There appear to be 5 clusters according to the pairwise representation

library(rgl)
#2.2)
#Find two differentmethods for representing the dataset
#Now how many clusters do you think?
#First we with represent it with a 3d graph
plot3d(x=fishData[,1],
       y=fishData[,2],
       z=fishData[,3])
#The 3d plot looks more like 6 or so clusters

#We will also plot the PCA
fishData.pca <- princomp(fishData)
fishData.pca
win.graph(800,600,10)
biplot(fishData.pca)
#The PCA plot looks more like 7 clusters to me. 
#So now after we have plotted the data in serval different
#tecniques, I think that k should be somewhere between 5 and 7

#2.3)
n <- nrow(fishData)
CH <- matrix(nrow=10, ncol=1)
#run from 2 to 10 as k
for (k in 2:10)
{
  print(paste("Test with k=",k))
  CH_loop <- matrix(nrow=5, ncol=1)
  #for each k, run 5 times
  for (i in 1:5)
  {
    print(paste("Test ",i))
    kmeans.result <- kmeans(fishData,k)
    kmeans.result
    #get the wss
    w <- (kmeans.result$tot.withinss/kmeans.result$totss)*100
    #get the bss
    b <- (kmeans.result$betweenss/kmeans.result$totss)*100
    #print wss
    print(paste("WSS=",round(w,2),"%"))
    #print bss
    print(paste("BSS=",round(b,2),"%"))
    #get ch index
    CH_index <- (b / (k - 1)) / (w / (n - k))
    CH_loop[i][1]=CH_index
    print(paste("CH index=",round(CH_index,2)))
    print("********")
  }
  #take the max ch_index
  max_CH <- max(CH_loop)
  CH[k][1] <- max_CH
  print(paste("max CH index=",round(max_CH,2)))
  print("##########")
}
CH
#According to the CH index, 7 is our appropriate number for k as it has the highest CH score

#Run the kmeans algorithm another time with k suggested by CH index
kmeans.result <- kmeans(fishData,7)

#Plot the dataset in 3d and use cluster for colors
plot3d(x=fishData[,1],
       y=fishData[,2],
       z=fishData[,3],
       col=kmeans.result$cluster)
#This is a very good representation of the data. Therefore, my final
#recommendation for the number of clusters is 7

#2.4)
#install package cluster
library(cluster)

#compute euc dist
fishEuc <- dist(fishData, method = "euclidean")
#run kmeans for k-1
kmeans6.result <- kmeans(fishData,6)
#run for k
kmeans7.result <- kmeans(fishData,7)
#run for k+1
kmeans8.result <- kmeans(fishData,8)
#for each of the 3 plot silhouette
#k-1
sk6 <- silhouette(kmeans6.result$cl,fishEuc)
plot(sk6)
#k
sk7 <- silhouette(kmeans7.result$cl,fishEuc)
plot(sk7)
#k+1
sk8 <- silhouette(kmeans8.result$cl,fishEuc)
plot(sk8)

#No, the silhouette does not agree with the CH index because the average sillouette width
#is higher with k+1 (k=8) than it is with our original k value. 
#This means that k of 8 is better because aech value is matched better with its cluster and further from different clusters
#than what our CH_index recommended

##################################
#Question 3) Arachnid
#3.1)
#import the dataset
arachnidsData <- read.csv("arachnids.csv")
#print the summary
summary(arachnidsData)
#plot the pairs representation
pairs(arachnidsData)
#print correlation coefficientfor everything but sex(because it is a factor)
cor(arachnidsData[,-c(4)])
#Is one variatle well correlated with age?
#v3 has the highest correlation with age, about double any other variable's correlation
#I don't think that the correlation is high enough with the other variables to use efficiently for a linear regression

#3.2)
#plot the pairwith with different colors for sex
pairs(arachnidsData,col=arachnidsData$sex)
#Do you see a difference?
#Yes, there is a very clear difference for most of the variables when you differentiate by sex

#divide the dataset into two subsets male and female
maleArachnids <- arachnidsData[arachnidsData$sex=="M",]
femaleArachnids <- arachnidsData[arachnidsData$sex=="F",]
#Print the correlation coefficients for each subset
cor(maleArachnids[,-c(4)])
cor(femaleArachnids[,-c(4)])
#Do you think it is possible now to obtain a good regression?
#Yes, the v3 variable in males has a .96 correlation with age and in females has .99 correlation with age.
#this is an excellent predictor and would work very easily in the model

#3.3)
#What will be the interesting variable for this model?
#v3 is our best predicting variable and is what we should use in our model

#For each sex subset, compute linear model for predicting age
maleReg <- lm(age~V3,data=maleArachnids)
femaleReg <- lm(age~V3, data=femaleArachnids)

#With these models can you predict age of a female arachnid with listed parameters
#yes, we will use the v3 to predict the age with our linear regression we just created
V3 <- 23
testArachnid <- data.frame(V3)
agePrediction <- predict(femaleReg,testArachnid)
agePrediction
#Our prediction is that the female arachnid is 21.7 months old