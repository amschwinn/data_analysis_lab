#Gradient Descent and Closed-Form Solution
#A mathmatical problem is said to have a closed form soultion iaof at least one
#solution of that problem can be expressed analytically in terms of a finite number
#of certain "well-known" functions
#To solve the linear regression problem in closed form, we are going to make use of the properties
#on the trace of a matrix, where if A is a nxn square matrix, the trace or the sum of the eigenvalues
#The closed form solution is given by: ?? = (X^T x X)^???1 x XTy
#Imagine that we want to optimize the function f(x)=1.2(x-2)^2 +3.2
#The gradient function is f'(x)=1.2 x 2(x-2)=2.4(x-2)
#If x=2 then 2.2(2-2)=0 so the actual solution we will approxiate with gradient descent is x=2

#Create some values
xs <- seq(0,4,len=20)

#Define the function we want to optimize
f <- function(x) {
  1.2*(x-2)^2 +3.2
}

#plot the function
plot(xs,f(xs),type="l",xlab="x",
     ylab=expression(1.2(x-2)^2 +3.2))

#calculate the gradient df/dx
grad <- function(x){
  1.2*2*(x-2)
}

lines(c(2,2),c(3,8),col="red",lty=2)
text(2.1,7,"Closedfrom_solution", col="red",pos=4)

#Gradiant descent implementation
#Initialize the first guess for x-value
x <- 0.1

#store x-values for graphing purposes (initial)
xtrace <- x

#Store y-values (function evaluated at x)
#for graphing purposes (initial)
ftrace <- f(x)
alpha <- .6 #learning rate 'alpha'
for(step in 1:100) {
  x <- x - alpha*grad(x) #Gradient descent update
  xtrace <- c(xtrace,x) #update for graph
  ftrace <- c(ftrace,f(x)) #updatae for graph
}

lines(xtrace,ftrace,type="b",col="blue")
text(.5,6,"Gradient_Descent",col="blue",pos="4")

#print the final value of x
print(x)
#Result: x converges to 2

#2 
#Logisitic Regression and Newton's Method
#Create some values for x
x <- seq(-5,5,length.out = 200)

#My function is an expression
myFunction <- expression(5*x^3-7*x^2-40*x+100)

#First derivative
derivativeF1 <- D(myFunction, 'x'); print(derivativeF1)

#Second derivative
derivativeF2 <- D(derivativeF1, 'x'); print(derivativeF2)

#Plot of my function, the first and second derivatives
y <- eval(myFunction)
yF1 <- eval(derivativeF1)
yF2 <- eval(derivativeF2)

plot(x,y,type = "l")
segments(-5,0,5,0, lty=3)
par(new = TRUE)
plot(x,y=yF1, type="l", col="red",ylim=range(y), ylab="")
par(new=TRUE)
plot(x,y=yF2, type = "l", col="blue", ylim=range(y), ylab = "")

#We will install and load the animation package and test Newton's method on some examples:
#x^2-4=0, 5*x^3-7*x^2-40*x+100=0,exp(-x)x=0 and atan(x)=0 (The last one will not converge)
#load the package
#install.packages("animation")
library(animation)

#Define the parameters that control the behaviour of the animation
#such as time interval, max num of animation frames, height and width,etc 
oopt = ani.options(interval =1, nmax = ifelse(interactive(),50,20))

#Define the symbol of the chart (here, with 20, a small dot)
par(pch=20)

#plot the Newton's method for a given function, an initial value (starting point),
#and a range for plotting the curve
#default example
xx = newton.method()
xx$root #solution

#take a long long journey
xx = newton.method(function(x)
  5*x^3-7*x^2-40*x+100, 7.15,
  c(-6.2,7.1))
xx$root

#another function
ani.options(interval = .5)
xx = newton.method(function(x) exp(-x) * x, rg=c(0,10), init = 2)
xx$root
