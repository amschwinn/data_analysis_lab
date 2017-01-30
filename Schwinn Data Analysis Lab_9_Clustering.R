#K-means

#With Iris dataset
setwd('C:/Users/amsch/Documents/MLDM/Data Analysis/Lab')
data("iris")
mydata <- iris[3:4]
class <- as.matrix(iris[5])

#number of columns and rows
n_c <- ncol(mydata)
n_r <- nrow(mydata)
dim <- n_r

#data preprocessing
#centered and reduced data after z-score transformations
mydata_zs <- mydata
#for(i in 1:n_c) {
  #use sapply function for computing directly the mean and standard deviation
  #on an attribute of a dataset
  #mydata_zs[i] <- ((mydata_zs[i]-sapply(mydata_zs[i],mean))/sapply(mydata_zs,sd))
#}
#however for loop is inefficeint so we use apply with normalize on each row
normalize <- function(row){(row-mean(row))/sd(row)}
mydata_zs <- apply(mydata,2,normalize)

#Reading the number of clusters
#most important part of kmeans is k (desired number of clusters or partitians at the end)
k <- 4
  #readline("Number_of_clusters?_")
print(paste("Clustering_in",k,"clusters"))
k <- as.integer(k)

#Random choice of the first centroids (the  means)
random_points <- sample(1:dim,k,replace = F)
means <- matrix(nrow=k,ncol = n_c)
for(i in 1:k) {
  for (j in 1:n_c) {
    means[i,j] <- mydata_zs[random_points[i],j]
  }
}

#Variable declaration and definition
clusters <- matrix(nrow=n_r,ncol=1,0)
prev_clusters <- matrix(nrow=n_r,ncol=1,0)
A <- matrix(nrow=n_c,ncol=1,0)
B <- matrix(nrow=n_c,ncol=1,0)
nb_loops <- 0
convergence_in_progress <- TRUE

#Programming a distance function
d <- function(X,Y){
  distance <- 0
  for(z in 1:n_c){
    distance <- distance + (X[z,1]-Y[z,1])^2
  }
  return(sqrt(distance))
}

#Graphical Representation
x <- 1:dim
plot(mydata_zs[x,3], mydata_zs[x,4], col=clusters[x,1])
par(new=TRUE) #on the same graph
y <- 1:k #the kmeans(in blue squares)
plot(means[y,3], means[y,4],col="blue",
     xlab="",ylab="",
     xlim=range(mydata_zs[x,3]),
     ylim=range(mydata_zs[x,4]), pch=15)

#1.2.8
#End of program
nb_loops <- 0
convergence_in_progress <- TRUE

###############
#Repeat until the convergence has not yet made #

win.graph(800,600,10)

while(convergence_in_progress) {
  #for each individual
  for(i in 1:dim){
    distances <- matrix(nrow=k, ncol=1, Inf)
    #For each attribute
    for(v in 1:n_c){
      #build point A
      A[v,1] <- mydata_zs[i,v]
    }
    #for each mean (Centroid)
    for(j in 1:k){
      #for each attribute
      for(v in 1:n_c){
        #Build the point B
        B[v,1] <- means[j,v]
      }
      #compute the distance between A and B
      distances[j,1] <- d(A,B)
    }
    #Initialization of the min distance to infinity
    dist_min <- Inf
    #and the number of clusters to zero
    cluster <- 0
    #For each mean point (Centroid)
    for(j in 1:k){
      #if the current distance is smaller than the min dist
      if(distances[j,1] < dist_min){
        #change to the new cluster number
        cluster <- j
        #and change the minimum dist
        dist_min <- distances[j,1]
      }
    }
    #Add the number of cluster to "clusters"
    clusters[i,1] <- cluster
  }
  nb_loops <- nb_loops +1
  #While the convergence is still in progress
  #Test wether the current clusters and those obtained before are the same
  the_same <- TRUE
  #for each individual
  for(i in 1:dim){
    #as they do not differ from one element, they are considered identical
    the_same <- the_same && (clusters[i,1]==prev_clusters[i,1])
  }
  #convergence must still be done if the identetiy is not found
  convergence_in_progress <- !(the_same)
  prev_clusters <- clusters
  ##### GRAPH######
  x <- 1:dim
  #Graph with the different collors for each cluster
  plot(mydata_zs[x,1], mydata_zs[x,2], col=clusters[x,1])
  #on the same graph
  par(new=TRUE)
  #presentation of the "k" means (in blue square)
  y <- 1:k
  plot(means[y,1],means[y,2],col="blue",
       xlab = "", ylab="",
       xlim=range(mydata_zs[x,1]),
       ylim=range(mydata_zs[x,2]), pch=15)
  print(paste("Results obtained in ", nb_loops, " loops."))
  readline("Press [ENTER] key to continue...")
  #Computation of the new means
  if(convergence_in_progress){
    means <- matrix(nrow=k,ncol = n_c,0)
    nb_indiv <- matrix(nrow = k,ncol = 1,0)
    #For each individual
    for(i in 1:dim){
      #we add its number to the clusters
      nb_indiv[clusters[i,1],1] <- nb_indiv[clusters[i,1],1] + 1
      #for each variable
      for(j in 1:n_c){
        #we add the coordinate of the points to the mean
        means[clusters[i,1],j] <- means[clusters[i,1],j] + mydata_zs[i,j]
        }
    }
    #for each mean
    for(i in 1:k){
      #for each variable
      for(j in 1:n_c){
        #we compute the mean
        means[i,j] <- means[i,j]/nb_indiv[i,1]
      }
    }
  }
}


