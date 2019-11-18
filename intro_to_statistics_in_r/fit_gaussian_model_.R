# The purpose of this program is to use the software R to fit Gaussian models,
# do model checking, test hypotheses, and construct confidence intervals

###################################################################################
# One sample Gaussian model
id<-1
set.seed(id) 
model<-sample(c(1:3),1)
cat("Model = ", model)
# Data are randomly generated from Gaussian distribution (model=1), Gamma distribution (model=2), or 
# Poisson distribution (model=3)
if (model==1) {
mu<-id-10*trunc(id/10)                                     # mu = last digit of ID
sigma<-max(1,trunc(id/10)-10*trunc(id/100))    # sig = second last digit of ID unless last digit is zero
cat("mu = ", mu, ", sigma = ", sigma)       # display values of mu and sigma
y<-sort(round(rnorm(30,mu,sigma),digits=2))  # 30 observations from G(mu,sig)
} else if (model==2) {
mu<-max(1,id-10*trunc(id/10))                     # mu = last digit of ID unless it is zero
y<-sort(round(rgamma(30,3,1/mu),digits=2))   # 30 observations from Gamma(3,1/mu)
sigma<-(3*mu^2)^0.5
cat("mu = ", mu, ", sigma = ", sigma)       # display values of mu and sigma
} else if (model==3) {
mu<-max(1,id-10*trunc(id/10))                     # mu = last digit of ID unless it is zero
y<- sort(round(rpois(30,mu),digits=2))   # 30 observations from Poisson(mu)
sigma<-mu^0.5
cat("mu = ", mu, ", sigma = ", sigma)       # display values of mu and sigma
} 
# Check Gaussian assumption using qqplot
qqnorm(y,xlab="Standard Normal Quantiles",main="Qqplot of Data")
qqline(y,col="red",lwd=1.5)  # add line for comparison
mu0<-mu+1
cat("mu0 = ", mu0)       # display value of mu0
# test hypothesis mu=mu0 and obtain 95% confidence interval for mu
t.test(y,mu=mu0,conf.level=0.95) 
#
sigma0<-sigma+2  
cat("sigma0 = ", sigma0)       # display value of sigma0
# test hypothesis sigma=sigma0 and obtain 95% confidence interval for sigma
df<-length(y)-1      # degrees of freedom
s2<-var(y)               
cat("sample variance = ", s2)          # display sample variance
chitest<-s2*df/sigma0^2
q<-pchisq(chitest,df)
cat("p-value for testing sigma=sigma0: ", min(2*q,2*(1-q)))       
p<-0.95                   # p=0.95 for 95% confidence interval
a<-qchisq((1-p)/2,df) # lower value from Chi-squared dist'n
b<-qchisq((1+p)/2,df) # upper value from Chi-squared dist'n
cat("95% confidence interval for sigma squared:  ",c(s2*df/b,s2*df/a))          
cat("95% confidence interval for sigma: ",c(sqrt(s2*df/b),sqrt(s2*df/a)))
###################################################################################

###################################################################################
# Linear regression model
set.seed(id)
x<-round(runif(100,1,20),digits=1)
alpha<-rnorm(1,0,5)
beta<- rnorm(1,0,5)
# display values of alpha and beta
cat("alpha = ", alpha, ", beta = ", beta)       
model<-sample(c(1:4),1)
cat("Model = ", model)
# Data are randomly generated depending on the value of the variable model
if (model==1) {
y<-round(alpha+beta*x+rnorm(100,0,10),digits=1)
} else if (model==2) {
y<-round(alpha+beta*x+rnorm(100,0,x),digits=1)
} else if (model==3) {
y<-round(alpha+beta*((x-10)/5)^2+rnorm(100,0,3),digits=1)
} else if (model==4)  {
y<-round(alpha+beta*x+3*rt(100,2),digits=1) }
# display sample correlation
cat("sample correlation = ", cor(x,y)) 
# run regression y = alpha+beta*x
RegModel<-lm(y~x)
summary(RegModel)     # parameter estimates and p-value for test of no relationship
alphahat<-RegModel$coefficients[1] # estimate of intercept
betahat<-RegModel$coefficients[2]  # estimate of slope
muhat<-RegModel$fitted.values      # fitted responses
r<- RegModel$residuals             # residuals
se<-summary(RegModel)$sigma        # estimate of sigma
rstar <- r/se                      # standardized residuals
# Scatterplot of data with fitted line
plot(x,y,col="blue")
title(main="Scatterplot with Fitted Line")
abline(a=alphahat,b=betahat,col="red",lwd=2)
# Residual plots
plot(x,rstar,xlab="x",ylab="Standardized Residual")
title(main="Residual vs x")
abline(0,0,col="red",lwd=1.5)
plot(muhat,rstar,xlab="Muhat",ylab="Standardized Residual")
title(main="Residual vs Muhat")
abline(0,0,col="red",lwd=1.5)
qqnorm(rstar,main="")
qqline(rstar,col="red",lwd=1.5)  # add line for comparison
title(main="Qqplot of Residuals")
# 95% Confidence interval for slope
confint(RegModel,level=0.95)
# 90% confidence interval for mean response at x=5
predict(RegModel,data.frame("x"=5),interval="confidence",lev=0.90)
# 99% Prediction interval for response at x=2
predict(RegModel,data.frame("x"=2),interval="prediction",lev=0.99)
# 95% confidence interval for sigma
df<-length(y)-2
a<-qchisq(0.025,df)
b<-qchisq(0.975,df)
cat("95% confidence interval for sigma: ",c(se*sqrt(df/b),se*sqrt(df/a)))
###################################################################################

###################################################################################
# Two sample Gaussian model
set.seed(id)
mu1<-rnorm(1,0,2)
mu2<-rnorm(1,0,2)
sigma<-max(1,trunc(id/10)-10*trunc(id/100))    # sig = second last digit of ID unless last digit is zero
# display values of mu1, mu2 and sigma
cat("mu1 = ", mu1, ", mu2 = ", mu2,", sigma = ", sigma) 
# Generate data      
y1<-sort(round(rnorm(30,mu1,sigma),digits=2))  # 30 observations from G(mu1,sig)
y2<-sort(round(rnorm(35,mu2,sigma),digits=2))  # 35 observations from G(mu2,sig)
# Test hypothesis of no difference in the means
t.test(y1,y2,mu=0,var.equal=TRUE,conf.level=0.95)
s1<-sd(y1)
s2<-sd(y2)
sp<-((29*s1^2+34*s2^2)/63)^0.5
cat("sample sd for sample 1 = ", s1, ", sample sd for sample 2 = ", s2)
cat("pooled estimate of sigma = ", sp) 
# Check Gaussian assumption using qqplots
qqnorm(y1,xlab="Standard Normal Quantiles",main="Qqplot of Data")
qqline(y1,col="red",lwd=1.5)  # add line for comparison
qqnorm(y2,xlab="Standard Normal Quantiles",main="Qqplot of Data")
qqline(y2,col="red",lwd=1.5)  # add line for comparison
###################################################################################