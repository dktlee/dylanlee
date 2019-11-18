###################################################################################
# Run this code only once 
library(MASS)     # truehist is in the library MASS
###################################################################################

###################################################################################
# Binomial confidence intervals and sampling distribution of likelihood ratio statistic
id<-1
set.seed(id)
#   generate a random value of theta
theta<-rbeta(1, max(1,id-10*trunc(id/10)), max(1,trunc(id/10)-10*trunc(id/100))) 
if (theta<0.1) {theta<-theta+0.1}   # avoid small values of theta
if (theta>0.9) {theta<-theta-0.1}   # avoid large values of theta
n<-30
cat("n = ",n," theta = ",theta)   # display values
# vector of observations for 5000 simulations from a Binomial(n,theta) distribution
yobs<- rbinom(5000,n,theta)
# corresponding vector of thetahat values
that<-yobs/n
# values used to construct an approximate 95% confidence interval  based on Gaussian approximation                               
pm<-1.96*sqrt(that*(1-that)/n)       
# each approximate 95% confidence interval is stored in a row of matrix cibi
cibi<-matrix(c(that-pm,that+pm),nrow=5000,byrow=F)
cibi[1:10,1:2]         # Look at first 10 approximate 95% confidence intervals
# display proportion of approximate 95% confidence intervals which contain true value of theta
prop<- mean(abs(theta-that)<pm)
cat("proportion of approximate 95% confidence intervals which contain true value of theta = ",prop)
#
# create function to calculate Binomial relative likelihood function
BinRLF <- function(x) {dbinom(y,n,x)/dbinom(y,n,thetahat)}
li<-rep(0,2*5000)
li<- matrix(li,ncol=2,byrow=TRUE)                   # initialize matrix to store likelihood intervals
# For the 5000 simulations determine 15% likelihood intervals which are also 
# approximate 95% likelihood intervals 
for (i in 1:5000) {
y<-yobs[i]
thetahat<-that[i]
if (thetahat==0) { li[i,1]<-0}        # if thetahat=0 then likelihood interval has left endpoint = 0
else {result<-uniroot(function(x) BinRLF(x)-0.15,lower=0,upper=thetahat)
li[i,1]<-result$root}
if (thetahat==1) { li[i,2]<-1}       # if thetahat=1 then likelihood interval has right endpoint = 1
else {result<-uniroot(function(x) BinRLF(x)-0.15,lower=thetahat,upper=1)
li[i,2]<-result$root}
}
li[1:10,1:2]         # Look at first ten 15% likelihood intervals 
# display proportion of 15% likelihood intervals which contain the true value of theta
prop<- mean(theta>=li[,1] & theta<=li[,2])
cat("proportion of 15% likelihood intervals which contain true value of theta = ",prop)
#
# calculate the likelihood ratio statistic for all 5000 simulations and plot a relative histogram of values
# the histogram approximates the sampling distribution of the likelihood ratio statistic
lambda<-(-2*log(dbinom(yobs,n,theta)/dbinom(yobs,n,that)))
truehist(lambda,h=0.5,xlab="Likelihood Ratio Statistic",main="Sampling Distribution of Likelihood Ratio Statistic")
curve(dchisq(x,1), from=0.001,to=12,add=TRUE,col="red",lwd=2) # superimpose Chi-squared (1) pdf
#
# use number of trials = 100 for the Binomial experiment
n<-100
cat("n = ",n," theta = ",theta)   # display values
# vector of observations for 5000 simulations from a Binomial(n,theta) distribution
yobs<- rbinom(5000,n,theta)
# corresponding vector of thetahat values
that<-yobs/n
# values used to construct an approximate 95% confidence interval  based on Gaussian approximation                               
pm<-1.96*sqrt(that*(1-that)/n)       
# each approximate 95% confidence interval is stored in a row of matrix cibi
cibi<-matrix(c(that-pm,that+pm),nrow=5000,byrow=F)
cibi[1:10,1:2]         # Look at first 10 approximate 95% confidence intervals for theta
# display proportion of approximate 95% confidence intervals which contain true value of theta
prop<- mean(abs(theta-that)<pm)
cat("proportion of approximate 95% confidence intervals which contain true value of theta = ",prop)
#
# For the 5000 simulations determine 15% likelihood intervals which are also
# approximate 95% likelihood intervals 
for (i in 1:5000) {
y<-yobs[i]
thetahat<-that[i]
if (thetahat==0) { li[i,1]<-0}       # if thetahat=0 then likelihood interval has left endpoint = 0
else {result<-uniroot(function(x) BinRLF(x)-0.15,lower=0,upper=thetahat)
li[i,1]<-result$root}
if (thetahat==1) { li[i,2]<-1}     # if thetahat=1 then likelihood interval has right endpoint = 1
else {result<-uniroot(function(x) BinRLF(x)-0.15,lower=thetahat,upper=1)
li[i,2]<-result$root}
}
li[1:10,1:2]         # Look at first ten 15% likelihood intervals 
# display proportion of 15% likelihood intervals which contain true value of theta
prop<- mean(theta>=li[,1] & theta<=li[,2])
cat("proportion of 15% likelihood intervals which contain true value of theta = ",prop)
#
# calculate the likelihood ratio statistic for all 5000 simulations and plot a relative histogram of values
# the histogram approximates the sampling distribution of the likelihood ratio statistic
lambda<-(-2*log(dbinom(yobs,n,theta)/dbinom(yobs,n,that)))
truehist(lambda,h=0.5,xlab="Likelihood Ratio Statistic",main="Sampling Distribution of Likelihood Ratio Statistic")
curve(dchisq(x,1), from=0.001,to=12,add=TRUE,col="red",lwd=2) # superimpose Chi-squared (1) pdf
###################################################################################


###################################################################################
# Exponential confidence intervals and sampling distribution of likelihood ratio statistic
set.seed(id)
theta<-max(1,id-10*trunc(id/10))                   # theta = last digit of ID unless it is zero
if (theta<1) {theta<-theta+1}   # avoid small values of theta
n<-20
cat("n = ",n," theta = ",theta)   # display values
ye<-rexp(5000*n,1/theta)    
# each of the 5000 rows of the matrix ye contains n independent observations from 
# Exponential(theta) distribution
ye<-matrix(ye,ncol=n,byrow=TRUE) 
that<-apply(ye,1,mean)                      # vector of 5000 means
pm<-1.96*that/sqrt(n)                     # used to get approximate 95% confidence interval 
# each approximate 95% confidence interval is stored in a row of matrix ciexp
ciexp<-matrix(c(that-pm,that+pm),nrow=5000,byrow=F)
ciexp[1:10,1:2]          # Look at first 10 approximate 95% confidence intervals 
# display proportion of approximate 95% confidence intervals which contain true value of theta
prop<- mean(abs(theta-that)<pm)
cat("proportion of approximate 95% confidence intervals which contain true value of theta = ",prop)
#
# create function to calculate Exponential relative likelihood function
ExpRLF<-function(x) {(thetahat/x)^n*exp(n*(1-thetahat/x))}
li<-rep(0,2*5000)
li<- matrix(li,ncol=2,byrow=TRUE)                   # initialize matrix to store likelihood intervals
# For the 5000 simulations determine 15% likelihood intervals which are also
# approximate 95% likelihood intervals 
for (i in 1:5000) {
thetahat<-that[i]
result<-uniroot(function(x) ExpRLF(x)-0.15,lower=max(0,thetahat-4*thetahat/(n^0.5)),upper=thetahat)
li[i,1]<-result$root
result<-uniroot(function(x) ExpRLF(x)-0.15,lower=thetahat,upper= thetahat+4*thetahat/(n^0.5))
li[i,2]<-result$root
}
li[1:10,1:2]         # Look at first ten 15% likelihood intervals 
# display proportion of 15% likelihood intervals which contain the value of theta
prop<- mean(theta>=li[,1] & theta<=li[,2])
cat("proportion of 15% likelihood intervals which contain true value of theta = ",prop)
#
# calculate the likelihood ratio statistic for all 5000 simulations and plot a relative histogram of values
# the histogram approximates the sampling distribution of the likelihood ratio statistic
lambda<- -2*log((that/theta)^n*exp(n*(1-that/theta)))
truehist(lambda,h=0.5,xlab="Likelihood Ratio Statistic",main="Sampling Distribution of Likelihood Ratio Statistic")
curve(dchisq(x,1), from=0.001,to=12,add=TRUE,col="red",lwd=2) # superimpose Chi-squared (1) pdf
###################################################################################


###################################################################################
# Gaussian confidence intervals
# The following R code runs a simulation in which 95% confidence intervals for the mean mu and the      
# standard deviation sigma are  calculated for 5000 randomly generated Gaussian data sets
set.seed(id)
mu<-id-10*trunc(id/10)                                    # mu = last digit of ID
sig<-max(1,trunc(id/10)-10*trunc(id/100))    # sig = second last digit of ID unless last digit is zero
cat("mu = ", mu, ", sigma = ", sig)       #display values of mu and sigma
yn<-rnorm(5000*25,mu,sig)               # generate G(mu,sig) observations
# each of the 5000 rows of the matrix yn contains 25 independent observations from a 
# G(mu,sig) distribution
yn<-matrix(yn,ncol=25,byrow=TRUE) 
ybar<-apply(yn,1,mean)                      # vector of 5000 means                       
s<-apply(yn,1,sd)                                  # vector of 5000 sample standard deviations
a<-qt(0.975,24)                # value from t tables for 95% confidence interval for mu 
pm<-a*s/sqrt(25)            # used to get 95% confidence interval for mu   
# each confidence interval for mu is stored in a row of matrix cimu
cimu<-matrix(c(ybar-pm,ybar+pm),nrow=5000,byrow=F)
cimu[1:10,1:2]       #Look at first ten 95% confidence intervals for mu
# proportion of 95% confidence intervals which contain the true value of mu
prop<- mean(abs(mu-ybar)<pm)
cat("proportion of 95% confidence intervals which contain true value of mu = ",prop)
# values from Chi-square distribution for 95% confidence interval for sigma
a<-qchisq(0.025,24)         
b<-qchisq(0.975,24)
# each confidence interval for sigma is stored in a row of matrix cisig
cisig<-matrix(c(sqrt(24*s^2/b), sqrt(24*s^2/a)),nrow=5000,byrow=F)
cisig[1:10,1:2]        #Look at first ten 95% confidence intervals for sigma
# proportion of 95% confidence intervals which contain the true value of sigma
prop<-mean(sig>=cisig[,1] & sig<=cisig[,2])
cat("proportion of 95% confidence intervals which contain true value of sigma = ",prop)
###################################################################################