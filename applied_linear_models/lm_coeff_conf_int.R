region<-c(1,1,1,1,2,2,2,2)
promotion<-c(1,2,3,4,1,2,3,4)
change<-c(2.1,4.6,3.9, 2.3, 0.2, 3.4, 1.7, -0.4)
Fregion<-factor(region)
Fpromo<-factor(promotion)
fit<-lm(change~Fregion+Fpromo)
summary(fit)
# if we let B1 = difference in change in sales between region 2 & 1 while promotion fixed
# B2 = difference in change in sales between promotion 2 & promo 3 while region fixed
# B3 = difference in change in sales between promo 3 & promo 1 while region fixed
# B4 = difference in change in sales between promo 4 & promo 1 while region fixed
# i.e., y = B0 + B1*D1 + B2*D2 + B3*D3 + B4*D4

fit2b<-lm(change~Fregion)
summary(fit2b)
anova(fit,fit2b)
qf(0.95,3,3) # 9.276628
# H0 : B2 = B3 = B4 = 0
# F = ((SSEr - SSEf)/p2) / MSEf
# since the f=statistic F = 21.229 and p-value = 0.015978, there is evidence to reject 
# H0 and so we conclude the change in sales are not all same among the 4 promos when region
# is fixed

fit2c<-lm(change~Fpromo)
summary(fit2c)
anova(fit,fit2c)
qf(0.95,1,3)
# H0 : B1 = 0
# since the F-statistic F = 40.678 and p-value = 0.0078, there is evidence to reject H0
# and so we conclude that the 2 regions have different change in sales when promo fixed

promoD<-relevel(Fpromo,2)
fit2d<-lm(change~promoD+Fregion)
summary(fit2d)
# se(B3hat-B2hat) = 0.4435, centered @ -1.2000
# we get that a 95% confidence interval is (-2.611,0.211) which contains the value 0

qt(0.975,3)
# B1hat = -2
# se(B1hat) = 0.3136
# we get that a 95% confidence interval is (-2.998,-1.002)

  
AAPL = read.table("AAPL.txt", header = TRUE)
attach(AAPL)
plot(Week,ClosePrice)
# we see that the plot implies a postive autocorrelation among the weekly closing
# stock price

fit3 = lm(ClosePrice ~ Week, data=AAPL)
summary(fit3) # can use the same code from prior assignments to determine
# coefficient estimates, now use summary(fit3) function
# B0 hat = 383.3015
# B1 hat = 6.0193
# yhati = 383.3015 + 6.0193*xi, where yi is the ClosePrice of week i,and 
# xi represents Week. 

plot(Week, residuals(fit3))
# To check the independence of the random erros, from the plot, it is 
# clear that the residuals are positively correlated.

library(lmtest)
dwtest(fit3, alternative = c("greater"), data=AAPL)
# we get that the test statistics are d=0.25237 and 
# p-value 2.2e -16. So we can reject the hypothesis of p=0
# (no autocorrelation) and are in favour of the alternative
# hypothesis p > 0 -> positive autocorrelation. This infers that the residuals
# are positively correlated

oldprice = head(AAPL$ClosePrice,-1) # to align yi and y(i-1)
currentprice = tail(AAPL$ClosePrice, -1)
plot(oldprice,currentprice)
# the plot of yi vs y(i-1) is evident of a positive linear trend, so we can 
# fit a linear model of yi vs y(i-1) for the relationship. 
fit3c = lm(currentprice ~ oldprice)
summary(fit3c)
# we get: yi hat = 22.73341 + 0.96544*y(i-1)

plot(Week, residuals(fit3c))
# the pattern in the residual plot is now more random vs the plot from part b
dwtest(fit3c, alternative = c("two.sided"))
# the test statistic of d = 2.0603 and p-value = 0.9433 indicate we can not reject 
# the null hypothesis of p=0 and that there is no evidence that the residuals are
# autocorrelated

# the model from part c is better (more appropriate) than the model we studied from
# part b. It should be noted that the residual plot against the predictor shows 
# an improvement. 


library(boot)
data(nuclear)
attach(nuclear)
plot(date,cost)
plot(t1,cost)
plot(t2,cost)
plot(cap,cost)
plot(cum.n,cost)
# possible linear relationships are between cost & date, cost & t1, cost & cap
# cost & t2 plot looks completely random which would mean no relationship
# cost & cum.n looks non-linear and may need a transformation

library(MASS)
fit4b<-lm(cost~date+t1+t2+cap+pr+ne+ct+bw+cum.n+pt, data=nuclear)
boxcox<-boxcox(fit4b, lambda=seq(-2,2, 0.1))
lambda<-boxcox$x
log<-boxcox$y
bestlambda<-lambda[which.max(log)] # =-.1010101
# fitting the multiple regression model and taking a look at the residual plots
# we can see that most of the residual vs predictor plots seem random
# but it should be noted that the cum.n plots is slightly U shaped. 
# we conclude there may be potential problems with the model assumptions
# and suggest data transformation using lambda = 0 -> lies in the 95% CI for lambda

fit4c = lm(log(cost) ~ log(date)+log(t1)+log(t2)+log(cap)+log(cum.n)+pr+ne+bw+ct+pt)
summary(fit4c)
# log(cost) = −61.82701 + 14.64918log(date) + 0.08614log(t1) + 0.28636log(t2) + 0.69516log(cap)
#             − 0.08118log(cum.n) − 0.09223pr + 0.25903ne + 0.03001bw + 0.12211ct − 0.21734pt
# we get the estimated effect of log(cap) is B4 hat = 0.69516
# 100*(exp(0.69516*log(1.01)) - 1) = 0.694105
# we expect 0.694% increase in the avg cost of construction of a LWR plant
# when the net capacity of the plant increase by 1%
# we get the estimated effect of ne is B6 hat = 0.25903
# 100*(exp(0.25903) - 1) = 29.56727
# so the cost of construction of a LWR plant is 29.56727% higher in the NE region
# compared to other regions

# the signs of the estimates seem reasonable. 
# postive: date, t1, and t2 makes sense since the longer it takes to get permits
# and license, cost increases. Also, cost increases if net capacity increases, 
# cooling tower is used, or if plant is constructed in NE region
# cost decreases as expereince of architect/engineer increases and if there is 
# already a prior LWR on site

plot(fitted(fit4b), residuals(fit4b))
qqnorm(residuals(fit4b))
qqline(residuals(fit4b))

plot(fitted(fit4c), residuals(fit4c))
qqnorm(residuals(fit4c))
qqline(residuals(fit4c))
# comparing the plots of residuals vs fitted values and Q-Q plots before and after
# taking the log-transformation, we can see that the log-transformed data gives
# no evidence of symmetric departure from the model we assumed. 
# we used the Box-Cox analysis to identify the proper transformation of the data
# to resolve problems with our model assumptions
