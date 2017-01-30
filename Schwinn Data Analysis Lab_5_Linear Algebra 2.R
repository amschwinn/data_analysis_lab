install.packages('expm')
library('expm')

#EigenValues and Eigenvectors
#Create a Hilbert matrix
H3 <- 1 / cbind ( seq ( 1 , 3 ) , seq ( 2 , 4 ) , seq ( 3 , 5 ) )
H3

#Eigenvalues and eigenvectors can be computed using the eigen() function
eigen(H3)

#Exercise
#1
#Calculate the matrix H = X(X^T X )^-1 X^T, where X is defined by:
X <- matrix(c(1,2,3,1,4,9),ncol=2)
H <- X%*%(solve(t(X)%*%X))%*%t(X)

#2
#Calculate the eigenvalue and eigenvectors
HE <- eigen(H)
HE

#3
#Calculate the trace of Matrix H and compare with the sum of the eigenvalues
trace <- function(data) sum(diag(data))
trace(H)

sum(HE$values)

#4
#Calculate the determinant of matrix H and compare with the product of the eigenvalues
det(H)

prod(HE$values)

#5
#Verify that the columns of X and I - H are eigenvectors of H, here HX = X and H(I -H) = 0
H%*%X
X

H%*%(diag(3)-H)

#Singular Value Decomposition of a Matrix
H3.svd <- svd(H3)
H3.svd

#We can verify these components can be multiplied in the appropriate way to reconstruct H3
H3.svd$u %*% diag(H3.svd$d) %*% t(H3.svd$v)

# H3^-1  = VD^-1 Ut
H3.svd$v %*% diag(1/H3.svd$d) %*% t(H3.svd$u)

#R Function apply()
#simple example of how to computer the sum of the rows of H3
apply(H3,1,sum)

#Exercise
#Consider the following circulant matrix
P <- rbind(seq(0.1,0.4,by=.1),c(0.4,seq(0.1,0.3,by=.1)),c(.3,.4,.1,.2),c(seq(0.2,0.4,by=.1),.1))
P

#P is an exapmle of a stoachastic matrix
apply(P,1,sum)

#Computer P^n for n=2,3,5,10. Is a pattern emerging?
P%^%2
P%^%3
P%^%5
P%^%10
#They all tend towards .25

#Exercise 2
#Part A
#Annual income for policies
claimInc <- c(10,30,50,100)

#Claim rates
claimRate <- c(0.1,.15,.03,.5)

#claim sizes
claimSize <- c(50,180,1500,250)

#total Matrix
#Goes in order of total policies, income, number of claims, claim amounts
totalMat <- matrix(c( 245921, 7304620, 34390.48, 6864693),ncol=1)

#calc claim amounts
claimAmount <- claimRate * claimSize
claimAmount

#translating the above to a system of linear equations
linSys <- rbind(rep(1,4),claimInc,claimRate,claimAmount)
linSys

#solve with the system of linear equations and totals to find the total number of each type of policy
policyTotals <- solve(linSys,totalMat)
#Answer to part A
policyTotals

#Part B
#Find the total income and total expected claim size for each type of policy
totalIncome <- policyTotals * claimInc
#Part B Answer 1
totalIncome

totalClaimSize <- policyTotals * claimAmount
#Part B Answer 2
totalClaimSize
