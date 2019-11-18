###################################################################################
# Run this code only once 
install.packages("moments")
library(moments)
library(MASS)     # truehist is in the library MASS
#
###################################################################################

###################################################################################
id<-1
mu<-id-10*trunc(id/10)    # last digit of ID
sig<-max(1,trunc(id/10)-10*trunc(id/100))    # sig = second last digit of ID unless last digit is zero
cat("mu = ", mu, ", sigma = ", sig)          # display values of mu and sigma
set.seed(id)
yn<-sort(round(rnorm(200,mu,sig),digits=2))  # 200 observations from G(mu,sig)
yn[1:5]                 # display first 5 numbers in the data set
# display sample mean and standard deviation
cat("sample mean = ", mean(yn), ", sample standard deviation = ", sd(yn))  
cat("five number summary: ",fivenum(yn))          # five number summary
cat("sample skewness = ", skewness(yn))      # sample skewness
cat("sample kurtosis = ", kurtosis(yn))          # sample kurtosis
# plot relative frequency histogram and superimpose Gaussian pdf
truehist(yn,main="Relative Frequency Histogram of Data")
curve(dnorm(x,mean(yn),sd(yn)),col="red",add=TRUE,lwd=2)
# plot Empirical and Gaussian cdf's
plot(ecdf(yn),verticals=T,do.points=F,xlab="y",ylab="ecdf",main="")
title(main="Empirical and Gaussian C.D.F.'s")
curve(pnorm(x,mean(yn),sd(yn)),add=TRUE,col="red",lwd=2) # superimpose Gaussian cdf
#############################################################################


#################################################################################
set.seed(id)
mu<-max(1,id-10*trunc(id/10))              # mu = last digit of ID unless it is zero
ye<-sort(round(rexp(200,1/mu),digits=2))   # 200 observations from Exponential(1/mu)
ye[1:5]                 # display first 5 numbers in the data set
# display sample mean and standard deviation
cat("sample mean = ", mean(ye), ", sample standard deviation = ", sd(ye))  
cat("five number summary: ",fivenum(ye))          # five number summary
cat("sample skewness = ", skewness(ye))      # sample skewness
cat("sample kurtosis = ", kurtosis(ye))          # sample kurtosis
# plot relative frequency histogram and superimpose Exponential pdf
truehist(ye,ymax=1/mean(ye),main="Relative Frequency Histogram of Data")
curve(dexp(x,1/mean(ye)),from=0.001,to=max(ye),col="red",add=TRUE,lwd=2)
# plot Empirical and Exponential cdf's
plot(ecdf(ye),verticals=T,do.points=F,xlab="y",ylab="ecdf",main="")
title(main="Empirical and Exponential C.D.F.'s")
curve(pexp(x,1/mean(ye)),col="red",add=TRUE,lwd=2)
#Plot side by side boxplots
boxplot(yn,ye,col="cyan",names=c("Gaussian Data","Exponential Data"))
###############################################################################


#################################################################################
set.seed(id)
yg<-sort(round(rgamma(200,3,1/mu),digits=2))   # 200 observations from Gamma(3,1/mu)
yg[1:5]                 # display first 5 numbers in the data set
cat("sample mean = ", mean(yg), ", sample standard deviation = ", sd(yg))  
cat("five number summary: ",fivenum(yg))          # five number summary
cat("sample skewness = ", skewness(yg))      # sample skewness
cat("sample kurtosis = ", kurtosis(yg))          # sample kurtosis
# plot relative frequency histogram and superimpose Gaussian pdf
truehist(yg,ymax=1/mean(yg),main="Relative Frequency Histogram of Data")
curve(dnorm(x,mean(yg),sd(yg)),col="red",add=TRUE,lwd=2)
# plot Empirical and Gaussian cdf's
plot(ecdf(yg),verticals=T,do.points=F,xlab="y",ylab="ecdf",main="")
title(main="Empirical and Gaussian C.D.F.'s")
curve(pnorm(x,mean(yg),sd(yg)),add=TRUE,col="red",lwd=2) # superimpose Gaussian cdf
###############################################################################


#################################################################################
set.seed(id)
x<-round(runif(100,0,20),digits=1)
alpha<-mean(yn)
beta<-mean(ye)
# display values of alpha and beta
cat("alpha = ", alpha, ", beta = ", beta)       
y<-round(alpha+beta*x+rnorm(100,0,beta*2),digits=1)
# display first 5 pairs of data
matrix(c(x[1:5],y[1:5]),nrow=5,ncol=2,byrow=F)  
# display sample correlation
cat("sample correlation = ", cor(x,y))        
plot(x,y,col="blue",main="Scatterplot of Data")
#################################################################################