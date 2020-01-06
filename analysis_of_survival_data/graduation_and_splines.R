mort <- read.csv("rates.csv")

#calc std error of q-hat
se <- sqrt(mort$q.hat_x*(1-mort$q.hat_x)/mort$E_x)
mort <- cbind(mort,se,upper = mort$q.hat_x + 1.96*se,lower = mort$q.hat_x - 1.96*se)

plot(mort$x, mort$q.hat_x, type = "l", main = "Crude vs Std Table Rates",
     xlab = "age", ylab = "q_x", ylim = c(min(mort$lower),max(mort$upper)))
lines(mort$x, mort$lower, lty = 2)
lines(mort$x, mort$upper, lty = 2)
lines(mort$x, mort$q.s_x,col="red")
legend("topleft", legend=c("crude", "95% CI", "STD Table"),
       lty=c(1, 2, 1), col =c("black", "black", "red"))


plot(mort$x, mort$q.hat_x/mort$q.s_x, type = "l", main = "Ratio of Crude and Std Table Rates",
     xlab = "age", ylab = "q-hat/q-s")
abline(h = 1.2, col="red")

#Use a = 1.195 and fit q-circle = a*q-s
qcircle <- 1.195*mort$q.s_x
resid1 <- (mort$q.hat_x*mort$E_x - qcircle*mort$E_x)/sqrt(qcircle*mort$E_x)
plot(resid1, main = "STD Table Graduation Residuals")

## STD Table Graduation Tests of Fit ####
#Chi-squared test
Q <- sum(resid1^2)
Q
#pvalue
1-pchisq(Q,df=19)

#Std residuals test
# Use 4 groups: (-infty, qnorm(0.25)) .... (qnorm(0.75), infty)
# Expected in each group is 5
sum(resid1 <= qnorm(0.25)) 
sum(resid1 > qnorm(0.25) & resid1 <= 0)
sum(resid1 > 0 & resid1 <= qnorm(0.75))
sum(resid1 > qnorm(0.75))

X <- sum((sum(resid1 <= qnorm(0.25))-5)^2/5,
(sum(resid1 > qnorm(0.25) & resid1 <= 0) -5)^2/5,
(sum(resid1 > 0 & resid1 <= qnorm(0.75)) -5)^2/5,
(sum(resid1 > qnorm(0.75)) -5)^2/5)
X
#pvalue
1-pchisq(X,df=3)

#sign test
s <- sum(resid1 > 0)
s
#pvalue
2*(1-pbinom(s-1,size=20,prob=0.5))

#change of sign test
z <- sum(diff(sign(resid1))!=0)
z
#pvalue
pbinom(z,size=19,prob=0.5)

#serial correlation test
c <- cor(resid1[1:19],resid1[2:20])
c
#pvalue
1-pnorm(c, mean = 0, sd = sqrt(1/19))

## Spline Graduation ###
#calc parameters:
A <- rbind(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
           c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
           c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
           c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
           c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 6, 36, 216),
           c(1, 3, 9, 27, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
           c(0, 0, 0, 0, 1, 5, 25, 125, 0, 0, 0, 0, 0, 0, 0, 0),
           c(0, 0, 0, 0, 0, 0, 0, 0, 1, 5, 25, 125, 0, 0, 0, 0),
           c(0, 1, 6, 27, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
           c(0, 0, 0, 0, 0, 1, 10, 75, 0, -1, 0, 0, 0, 0, 0, 0),
           c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 10, 75, 0, -1, 0, 0),
           c(0, 0, 2, 18, 0, 0, -2, 0, 0, 0, 0, 0, 0, 0, 0, 0),
           c(0, 0, 0, 0, 0, 0, 2, 30, 0, 0, -2, 0, 0, 0, 0, 0),
           c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 30, 0, 0, -2, 0),
           c(0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0),
           c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,36))

B <- matrix(c(0.11,0.148,0.233,0.322,0.298,0.148,0.233,0.322,0,0,0,0,0,0,0,0), ncol=1)

param <- solve(A)%*%B
round(param,5)

#for plotting
knotx <- c(80,83,88,93,99)
knotq <- c(0.110,0.148,0.233,0.322,0.298)
sp <- splinefun(knotx, knotq, method = "natural")(80:99)
plot(mort$x, mort$q.hat_x, type = "l", main = "Crude vs Spline Graduated Rates",
     xlab = "age", ylab = "q_x")
lines(mort$x, sp,col="red")
legend("topleft", legend=c("crude", "fitted spline"), lty = c(1,1),
       col =c("black", "red"))


## Spline Graduation Tests of Fit ####
resid2 <- (mort$q.hat_x*mort$E_x - sp*mort$E_x)/sqrt(sp*mort$E_x)

#Chi-squared test
Q2 <- sum(resid2^2)
Q2
#pvalue
1-pchisq(Q2,df=20-5-3)

#Std residuals test
# Use 4 groups: (-infty, qnorm(0.25)) .... (qnorm(0.75), infty)
# Expected in each group is 5
sum(resid2 <= qnorm(0.25)) 
sum(resid2 > qnorm(0.25) & resid2 <= 0)
sum(resid2 > 0 & resid2 <= qnorm(0.75))
sum(resid2 > qnorm(0.75))

X2 <- sum((sum(resid2 <= qnorm(0.25))-5)^2/5,
         (sum(resid2 > qnorm(0.25) & resid2 <= 0) -5)^2/5,
         (sum(resid2 > 0 & resid2 <= qnorm(0.75)) -5)^2/5,
         (sum(resid2 > qnorm(0.75)) -5)^2/5)
X2
#pvalue
1-pchisq(X2,df=3)

#sign test
s2 <- sum(resid2 > 0)
s2
#pvalue
2*(pbinom(s2,size=20,prob=0.5))

#change of sign test
z2 <- sum(diff(sign(resid2[which(resid2!=0)]))!=0) #ignore knots which are exactly 0
z2
#pvalue
pbinom(3,size=14,prob=0.5)

#serial correlation test
c2 <- cor(resid2[1:19],resid2[2:20])
c2
#pvalue
1-pnorm(c2, mean = 0, sd = sqrt(1/19))
