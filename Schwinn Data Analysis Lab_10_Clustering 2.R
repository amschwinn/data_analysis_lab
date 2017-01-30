#clustering iris data
#loading the dataset
data("iris")
mydata <- iris[1:4]
class <- as.matrix(iris[5])

#k-means clustering
#with k=3
kmeans.result <- kmeans(mydata,3)

#summary indicates result of kmeans algo
summary(kmeans.result)
#cluster: number from 1 to k indicating the cluster to which each pair is allocated
#centers: coordinates of the centroids (the means)
#totss: total sum of squares
#withinss: within-cluster sum of squares (within each cluster)
#tot.withinss: total within-cluster sum of squares
#betweenss: between-cluster sum of squares
#size: number of points in each cluster

#Clustering stability
#print the contingency table of real classes of iris and cluster from kmeans
print(table(class,kmeans.result$cluster))

#run the kmeans algo 10 times on whole iris dataset.
#print each time the contingency table but the within-cluster sum of squares (WSS)
#and the between-cluster sum of squares (BSS)
w <- (kmeans.result$tot.withinss/kmeans.result$totss)*100
b <- (kmeans.result$betweenss/kmeans.result$totss)*100
print(paste("WWS=",round(w,2),"%"))
print(paste("BBS=",round(b,2),"%"))
print(table(class,kmeans.result$cluster))

#Hierarchial Clustering
hc <- hclust(dist(mydata), "ave")
summary(hc)

win.graph(800,600,10)
plot(hc, hang = -1, labels = class)

#too much data to visualize efficiently so we take a sample and run it through the algo
idx <- sample(1:dim(iris)[1],40)
irisSample <- iris[idx,]
irisSample$Species <- NULL
hc <- hclust(dist(irisSample), method = "ave")
win.graph(800,600,10)
plot(hc,hang=-1,labels=iris$Species[idx])

#cut the dendrogram in 3
rect.hclust(hc, k=3)
groups <- cutree(hc,k=3)
groups

##########################################
#2) Clustering Ruspini Dataset
rm(list = ls())
mydata <- read.table("ruspini.txt", quote="\"")

#K-means clustering and internal clustering validation
#unlike iris dataset, we don't know how many clusters we have to find in ruspini
#There is no ground truth
for(k in 2:10){
  rKMeans <- kmeans(mydata,k)
  B_H_index <- rKMeans$withinss/k
  #Need to figure out what n is. Number of obvs?
  C_H_index <- (rKMeans$betweenss/(k-1))/(rKMeans$withinss/(n-k))
  #append it all to a matrix based and pick best observation
}