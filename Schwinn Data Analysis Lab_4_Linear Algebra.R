########
#1 Introduction to Computational Linear Algebra
#Plot the lines l1 and l2 corresponding to the 
#equations 1 and 2 and solve them graphically
#(1) 3x1 ??? 4x2 = 6
#(2) x1 + 2x2 = ???3
#(1) ??? 4x2 = 3x1 ??? 6 ??? x2 = (3x1 ??? 6)/4
#(2) ??? 2x2 = ???x1 ??? 3 ??? x2 = (???x1 ??? 3)/2

x1 <- seq(-5,5, length=200)
l1 <- (3*x1-6)/4
l2 <- (-x1-3)/2

plot(x1, l1, col = "blue", type="l",lwd=1,ylim=range(c(l1,l2)))
par(new=TRUE)
plot(x1,l2,col="red",type="l",lwd=1,ylim=range(c(l1,l2)))

#The solution is where they intersect so let's plot it
segments(-5,-1.5,0,-1.5, col = "black", lty=3)
segments(0,-5,0,-1.5,col="black",lty=3)

#Solving Matrix
#matrices are inverted and linear systems of equations are solved using the solve() (by using a
#method based on the LU decomposition) or qr.solve() (by using a method based on the QR decomposition)
#functions. The matrix multiplication can be performed using the operator %*%
A     <- matrix(c(3,1,-4,2),ncol=2); print(A)
Ainv  <- solve(A); print(Ainv)
b     <- matrix(c(6,-3), ncol=1); print(b)
x     <- Ainv %*% b; print(x)

#Problem
#Find the solutions of the following equations
# 1) 3x1 + 2x2 = 7
# 2) x1 ??? 3x2 = ???5
A     <- matrix(c(3,1,2,-3), ncol=2); print(A)
Ainv  <- solve(A); print(A)
b     <- matrix(c(7,-5), ncol = 1); print(b)
x     <- Ainv %*% b; print(x)
#This tells us that our solution is x1 = 1 and x2 = 2

#So let's check this solution
x1 <- 1
x2 <- 2
3*x1 + 2*x2
x1 - 3*x2
#The solutions check out. This process works

#2 Vectors and Matrices
#Constructing Matrix Objects
#Matrices can be constructed using the functions matrix() (for creating a matrix from the given set of
#values), cbind() or rbind()
H3 <- matrix ( c ( 1 , 1 / 2 , 1 / 3 , 1 / 2 , 1 / 3 , 1 / 4 , 1 / 3 , 1 / 4 , 1 / 5 ) , nrow= 3 )
H3

#Using cbind to accomplish the same thing as above
H3b <- 1 / cbind ( seq ( 1 , 3 ) , seq ( 2 , 4 ) , seq ( 3 , 5 ) )
H3b

#The seq function will generate a sequence, for example seq(1,3) will produce the results 1 2 3 (similar
# as 1:3). In this example, rbind() would give the same result, because of symmetry.
X <- matrix(seq(1,12), nrow = 3)
X

#Another useful function for creating matrices in is diag(). It extracts or replaces the diagonal of a
#matrix, or constructs a diagonal matrix. This function is used for creating identity matrices (a square matrix
#of size n × n with ones on the main diagonal and zeros elsewhere)
diag(5)

#Exercise
#Use the matrix(), seq() and rep() functions to construct the following 5 × 5 Hankel matrix:
HME <- matrix(c(seq(1,5),seq(2,6),seq(3,7),seq(4,8),seq(5,9)), nrow=5)
HME
#OR
HME <-  matrix(rep(seq(0,4),5),nrow=5) +
        matrix(rep(seq(1,5),5), byrow = TRUE, nrow=5)

HME

#Accessing Matrix Elements
#Indexing of matrix elements is the same as for data frames: the (i, j) element is located in the i
#th row andjth column. For example, the (3, 2) element of X is 6.
X[3,2]

#Rows and Column Names
#It is possible for the rows and/or columns of a matrix to have individual names with rownames() and
#colnames() functions.
colnames(X) <- c("X1","X2","Y","Z")
rownames(X) <- c("obs1","obs2","obs3")
X

X["obs1", "Y"]
X[,"Z"]
X["obs2",]

#Exercise
#Construct the two vectors of heights (in cm) and weight (in kg) for 5 individuals
height        <- c(172,168,167,175,180)
weight        <- c(62,64,51,71,69)
EM            <- cbind(height,weight)
rownames(EM)  <-c("Neil","Cindy","Pardeep","Deepak","Hao")
EM

#Matrix Properties
#The dimension of a matrix is its number of rows and its number of columns
dim(X)

#The determinant of a 2 × 2 matrix [acbd] matrix can be calculated as ad ??? bc. For larger square matrices,
#the calculation becomes more complicated.
det(H3)

#The diagonal elements can be obtained using the diag()
diag(X)
diag(H3)

#We can then compute the trace (the sum of the diagonal entries) using a home-made function
trace <- function(data) sum(diag(data))
trace(X)
trace(H3)

#The t() function is used to calculate the matrix transpose XT
Xt <- t(X)
Xt


#Triangular Matrices
#The functions lower.tri() and upper.tri() can be used to obtain the lower and upper triangular parts of
#matrices. The output of the functions is a matrix of logical elements, with TRUE representing the relevant
#triangular elements.
lower.tri(H3)
upper.tri(H3)

#Matrix Arithmetic
#Multiplication of a matrix by a scalar constant is the same as multiplication of a vector by a constant. For
#example, using the X matrix from the previous section, we can multiply each element by 2
Y <- 2*X
Y

#Elementwise addition of matrices also proceeds as for vectors.
YX <- Y+X
YX

#The command X * Y performs elementwise multiplication. Note that this differs from the usual form of
#matrix multiplication that we will discuss below

XY <- X*Y
XY

#Matrix Multiplication and Inversion
#If A and B are matrices, then the matrix product AB is the matrix representing the composition of the
#two operations: first apply B, then apply A to the result. For matrix multiplication to be a properly defined
#operation, the matrices to be multiplied must conform. That is, the number of columns of the first matrix must
#match the number of rows of the second matrix. The resulting matrix AB will have its row dimension taken
#from A and its column dimension taken from B. In , this form of matrix multiplication can be performed
#using the operator %*%
t(Y) %*% X

#From the previous section we saw that t(Y) has three columns and X has three rows, so we can perform
#the multiplication Y
#T X . The result is a 2 × 2 matrix, since t(Y) has two rows and X has two columns. If
#we failed to transpose Y, we would obtain an error.
#The crossprod() function is a somewhat more efficient way to calculate Y
#T X
crossprod(Y,X)

#Matrix Inversion
#In , matrices are inverted and linear systems of equations are solved using the solve() or qr.solve()
#functions. solve() uses a method based on the LU decomposition; qr.solve() is based on the QR decomposition.
H3inv <- solve(H3)
H3inv

#To verify that this is the inverse of H3, we can check that the product of H3inv and H3 is the 3 × 3
#identity:
H3inv %*% H3

#Solving linear systems
#The function solve(A, b) gives the solution to systems of equations of the form Ax = b. For example,
#let us find x such that H3x = b where H3 is the 3 × 3 Hilbert matrix and b = [123]T
b <- c(1,2,3)
x <- solve(H3,b)
x

#Exercise
#Let [x1, x2, x3, x4, x5, x6]T = [10, 11, 12, 13, 14, 15]T
#Find the coefficients of the quintic polynomial f(x) for which
#[f(x1), f(x2), f(x3), f(x4), f(x5), fx6)]T = [25, 16, 26, 19, 21, 20]T
#The quintic polynomial f(x) = a0 + a1x + a2x2 + a3x3 + a4x4 + a5x5
#can be viewed as the matrix product of the row vector [1, x, x2, x3, x4, x5] with the column vector [a0, a1, a2, a3, a4, a5]T.
#Work out the matrix version of this to give [f(x1), f(x2), f(x3), f(x4), f(x5), f(x6)]T.
X1 <- seq(10,15)
X2 <- X1^2
X3 <- X1^3
X4 <- X1^4
X5 <- X1^5
X0 <- c(rep(1,6))
A <- cbind(X0, X1, X2, X3, X4, X5)
A
f <- matrix(c(25, 16, 26, 19, 21, 20), nrow=6)
a <- solve(A, f)
a
# f(x) = 253610 - 102551 x + 16500.92 x^2 
#      - 1320.667 x^3 + 52.58333 x^4 - 0.8333 x^5

A %*% a
