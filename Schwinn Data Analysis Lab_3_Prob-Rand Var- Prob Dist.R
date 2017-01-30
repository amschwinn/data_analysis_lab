install.packages("ggplot2")
install.packages("stats")

#####################
# 1) Probabilities
require(stats)
for(i in 1:10) {
  x <- round(runif(1))
  print(x)
}

proba <- as.vector(1:1000)
sum   <- 0
for (i in 1:1000) {
  x   <- round(runif(1))
  sum <- sum + x
  proba[i] <- sum / i
}

plot(1:1000, proba[1:1000], "l", xlim=c(1,1000), ylim=c(0,1))
segments(0,0.5,1000,0.5, col = "red")

#Modify from 1,000 tosses to 10k tosses
proba <- as.vector(1:10000)
sum   <- 0
for (i in 1:10000) {
  x   <- round(runif(1))
  sum <- sum + x
  proba[i] <- sum / i
}

plot(1:10000, proba[1:10000], "l", xlim=c(1,10000), ylim=c(0,1))
segments(0,0.5,1000,0.5, col = "red")
#The larger dataset of 10k moves much closer to the equilibrium of
#balanced heads and tails results than the original 1k flips
######################################
# 2 Random Variable and Probability Distributions
max       <- 6
nb_events <- max
proba     <- as.vector(1:max)
for (i in 1:max) {
  proba[i] <- 1/nb_events
}

require(ggplot2)
bar <- qplot(factor(1:max),proba[1:max],
      xlab="Value_obtained_by_one_die",
      ylab="Probability")
bar + geom_bar(stat="identity")

#now with 2 dice
max_dice <- 6
score_max <- max_dice * 2
nb_events <- max_dice ^ 2

proba <- rep.int(0,score_max)
proba <- as.vector(proba)

for ( i in 1:max_dice) {
  for ( j in 1:max_dice) {
    proba [i + j] <- proba[i+j] + 1/nb_events
  }
}

dd_plot <- qplot(factor(1:score_max),proba[1:score_max],
     xlab = "Value_obtained_with_the_sum_of_two_dice",
     ylab = "Probability")
dd_plot + geom_bar(stat="identity")

#2.1.1 Expected Value of a Discrete Random Variable
expec <- 0
for (i in 1:score_max) {
  expec <- expec + proba[i] * i
}
print(expec)

#The expected value was 7. This makes sense because
#it is the sum combination that has the highest likelyhood
#of being rolled

#2.1.2 Variance and Standard Deviation of the sum of the two dice
variance <- 0
for(i in 1:score_max) {
  variance <- variance + (expec - i)^2 * proba[i]
}
print(variance)
st_deviation <- sqrt(variance)
print(st_deviation)

#There is another way to computer the variance....
expec_square <- 0
for(i in 1:score_max) {
  expec_square <- expec_square + proba[i] * i^2
}
variance_bis <- expec_square - (expec^2)
print(variance_bis)

u_variance <- variance * (nb_events / (nb_events - 1))

#2.1.3 R Statistical Functions
sum <- rep.int(0,nb_events)
sum <- as.vector(sum)
event <- 0

for (i in 1:max_dice) {
  for(j in 1:max_dice) {
    event <- event + 1
    sum[event] <- i + j
  }
}

hist(sum, breaks = c(1:12), col = "blue1")
mean(sum)
var(sum)
sd(sum)

#2.1.4 Correlation Coefficient
table(revision_grade)
cor(revision_grade)

#2.2 Probability Distribution for Continuous Random Variables
data("iris")
summary(iris)

pairs(iris[-5], bg=iris$Species, pch=21)
cor(iris[-5])

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(iris[-5], bg=iris$Species, pch=21)

#2.3 Binomial and Geometric Distributions
#2.3.1 Binomial Distribution
dbinom(x=4, size=6, prob=0.5)

pbinom(4,6,.5)

#Plotting the probability mass function of the binomial distribution
colors  <- c("black","blue","red","green")
n_max   <- 20
n       <- 5
p       <- 1/2
fd      <- function(x) {
  dbinom(x,n,p)
}

plot(cbind(0:n, sapply(0:n, fd)),
  xlim=c(0,n_max), ylim=c(0,.40), type = "p",
  ylab = "", pch=15, cex=2, col=colors[1], cex.axis=2)

n <- 10
fd <- function(x) {
  dbinom(x,n,p)
}

points(cbind(0:n, sapply(0:n,fd)),
       xlim=c(0,n_max), ylim=c(0,.40), type = "p",
       ylab = "", pch=16, cex=2, col=colors[2], cex.axis=2)

n <- 15 
fd <- function(x) {
  dbinom(x,n,p)
}

points(cbind(0:n, sapply(0:n,fd)),
       xlim=c(0,n_max), ylim=c(0,.40), type = "p",
       ylab = "", pch=17, cex=2, col=colors[3], cex.axis=2)

n <- 20
fd <- function(x) {
  dbinom(x,n,p)
}

points(cbind(0:n, sapply(0:n,fd)),
       xlim=c(0,n_max), ylim=c(0,.40), type = "p",
       ylab = "", pch=18, cex=2, col=colors[4], cex.axis=2)

mtext(c(expression(paste(italic(n), "=5"))),
      adj=0, at=1, col=colors[1])

mtext(c(expression(paste(italic(n), "=10"))),
      adj=0, at=3, col=colors[2])

mtext(c(expression(paste(italic(n), "=15"))),
      adj=0, at=5, col=colors[3])

mtext(c(expression(paste(italic(n), "=20"))),
      adj=0, at=7, col=colors[4])

title(main=c(expression(paste("Binomial_distribution_for_",
                               italic(p), "_=_1/2_and_",
                               italic(n), "_=_5,_10,_15,_and,_20"))),
       sub="Probability_mass_function")

#Randomly choosing test answer probability
#Suppose there are twelve multiple choice questions in an English class quiz. Each question has five
#possible answers, and only one of them is correct. Find the probability of having four or less correct answers
#if a student attempts to answer every question at random.

#the probability of getting 4 answers correct out of 5 randomly guessing
dbinom(4, size = 12, prob = .2)

#The probability of 4 or less questions answered correctly in a 12 question multiple choice
pbinom(4, size=12, prob = .2)

# 2.3.2 Geometric Distribution

#2.4 Poisson Distribution
#Problem: If there are twelve cars crossing a bridge per minute on average, find the probability of having seventeen
#or more cars crossing the bridge in a particular minute
ppois(16, lambda = 12, lower=FALSE)
#The probability of 17 or more cars on the brdge at a time is ~10%
#We use the upper tail test by making the lower parameter false
#This is because we are trying to find for MORE than 16 at a time

#2.5 Normal Distribution
#Plot the standard normal function
x=seq(-4,4,length=200)
y=1/sqrt(2*pi)*exp(-x^2/2)
plot(x,y,type = "l",lwd=5,col="blue")

par(new=TRUE)
y=dnorm(x,mean=0,sd=1)
plot(x,y,type="l",lwd=1,col="yellow")

x <- seq(-10,100,.1)
normdensity1 <- dnorm(x,mean=10,sd=5)
normdensity2 <- dnorm(x,mean=40,sd=2.5)
normdensity3 <- dnorm(x,mean=70,sd=10)

plot(x,normdensity1, type = "l", col="red",
     ylim=range(c(normdensity1,normdensity2,normdensity3)))
par(new=TRUE)
plot(x,normdensity2, type = "l", col="green",
     ylim=range(c(normdensity1,normdensity2,normdensity3)),
     axes = FALSE, xlab="",ylab = "")
par(new=TRUE)
plot(x,normdensity3, type = "l", col="blue",
     ylim=range(c(normdensity1,normdensity2,normdensity3)),
     axes = FALSE, xlab="",ylab = "")

x=seq(-4,4,length=200)
y=dnorm(x)
plot(x,y,type = "l", lwd=2, col="blue")
x=seq(-1,1,length=100)
y=dnorm(x)
polygon(c(-1,x,1),c(0,y,0), col="gray")

pnorm(1,mean=0,sd=1)-pnorm(-1,mean=0,sd=1)

x=seq(-4,4,length=200)
y=dnorm(x)
plot(x,y,type = "l", lwd=2,col="blue")
x=seq(-2,2,length=200)
y=dnorm(x)
polygon(c(-2,x,2),c(0,y,0),col="gray")
pnorm(2,mean=0,sd=1)-pnorm(-2,mean=0,sd=1)
pnorm(3,mean=0,sd=1)-pnorm(-3,mean=0,sd=1)

#Problem
#A normal distribution with mean = 3500 grams and standard deviation = 600 grams is a reasonable model
#for the probability distribution of the continuous variable X: birth weight of a randomly selected full-term
#baby

#What proportion of birth weights are between 2900 and 4700 grams?
pnorm(4700,mean=3500,sd=600)-pnorm(2900,mean=3500,sd=600)
#81.8%

#What birth weight w is exceeded only in 2.5% of the cases?
qnorm(1-2.5/100, mean = 3500, sd = 600)
#OR by taking the upper tail
qnorm(2.5/100,mean=3500,sd=600, lower.tail = FALSE)
#The answer is 2.5% of cases exceed 
#4675.9 grams

#show this answer graphically
x <- seq(0,7000,length=200)
y <- dnorm(x, mean = 3500, sd = 600)
plot(x,y,type = "l", lwd=2, col="gray")
z <- qnorm(2.5/100, mean = 3500, sd = 600, lower.tail = FALSE)
x <- seq(z,7000,length=100)
y <- dnorm(x,mean = 3500, sd=600)
polygon(c(z,x,7000),c(0,y,0), col = "navy")
mtext("What_birth_weight_is_exceeded_only_in_2.5%_ of_the_cases?",
      side=1, adj=1,col = "navy")

#Student Scores Examples
x     <- seq(1:25)
plot(x,scores[x,1])
hist(scores[x,1])
mu    <- mean(scores[,1])
sigma <- sd(scores[,1])
x2=seq(20,80,length=200)
par(new=TRUE)
y=dnorm(x2,mean=mu,sd=sigma)

plot(x2,y,type = "l", lwd =1, col="red", xlab="", ylab="",yaxt="n")

#How many students do you expect to find with a score equal or greater than 55, 60, 65 or 70? How many
#are they in reality?

#For over the score of 55
round(25 * (pnorm(55,mean=mu,sd=sigma,lower.tail = FALSE)))
#8

#For 60
round(25 * (pnorm(60,mean=mu,sd=sigma,lower.tail = FALSE)))
#4

#For 65
round(25 * (pnorm(65,mean=mu,sd=sigma,lower.tail = FALSE)))
#2

#For 70
round(25 * (pnorm(70,mean=mu,sd=sigma,lower.tail = FALSE)))
#1

