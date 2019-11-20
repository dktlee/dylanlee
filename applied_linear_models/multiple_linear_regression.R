mydata = read.table("wine.txt", header = TRUE)
attach(mydata)
n = nrow(mydata)
X = cbind(rep(1,n), Clarity, Aroma, Body, Flavor, Oakiness)
t(X)%*%X
vmatrix = solve(t(X)%*%X)
Bhat = solve(t(X)%*%X)%*%t(X)%*%Quality
fit = lm(Quality~Clarity+Aroma+Body+Flavor+Oakiness)
fit$coef

# Then we have least square estimates:
# B0hat       B1hat       B2hat       B3hat       B4hat       B5hat
# 5.1068792   1.9438139   0.6523109   0.8756675   0.6752492   -1.2128137
# the fitted equation is:
# yhat = 5.1068792 + 1.9438139*Clarify + 0.6523109*Aroma + 0.8756675*Body + 0.6752492*Flavor - 1.2128137*Oakiness

yhat = X%*%Bhat
r = Quality - yhat
sig2hat = t(r)%*%r/(n-5-1)
sighat = sqrt(sig2hat)
se0 = sighat * sqrt(as.vector(vmatrix[1,1])) # = 3.994572
se1 = sighat * sqrt(as.vector(vmatrix[2,2])) # = 3.070763
se2 = sighat * sqrt(as.vector(vmatrix[3,3])) # = 0.369912
se3 = sighat * sqrt(as.vector(vmatrix[4,4])) # = 0.5805216
se4 = sighat * sqrt(as.vector(vmatrix[5,5])) # = 0.5093382
se5 = sighat * sqrt(as.vector(vmatrix[6,6])) # = 0.483003
# Test Statistics
t0 = Bhat[1,1]/se0 # = 1.278455
t1 = Bhat[2,1]/se1 # = 0.6330067
t2 = Bhat[3,1]/se2 # = 1.763422
t3 = Bhat[4,1]/se3 # = 1.508415
t4 = Bhat[5,1]/se4 # = 1.325738
t5 = abs(Bhat[6,1]/se5) # = |-2.510986| = 2.510986
qt(0.975,32) # = 2.036933
# p-values
p0 = 2*(1 - pt(t0, df = n-5-1)) # = 0.2102843
p1 = 2*(1 - pt(t1, df = n-5-1)) # = 0.5312273
p2 = 2*(1 - pt(t2, df = n-5-1)) # = 0.08737707
p3 = 2*(1 - pt(t3, df = n-5-1)) # = 0.1412591
p4 = 2*(1 - pt(t4, df = n-5-1)) # = 0.1943131
p5 = 2*(1 - pt(t5, df = n-5-1)) # = 0.017285

# Therefore, using the t-test to evaluate the significance of each regression coefficient at 5% sig. level, 
# since all p-values are >= 0.05, and additionally all t-statistics are less that the quartile at a 5% sig. level,
# there is no evidence to reject the null hypothesis of no relationship for any covariate
# we can conclude that all covariates seem to be important in determining the quality of the wine
# oakiness has the highest p-value, and so it seems that it is the most important in determining quality of the wine
# 1 unit increase in oakiness contributes to -1.2128137 in quality of wine

summary(fit)
# Multiple R-squared:  0.4931,	Adjusted R-squared:  0.4139

sigma2B2B3 = sig2hat*as.vector(vmatrix[3,3]) + sig2hat*as.vector(vmatrix[4,4]) - 2*sig2hat*as.vector(vmatrix[3,4])
tstatc = abs(Bhat[3,1] - Bhat[4,1]) / sqrt(sigma2B2B3) # 0.3056672 = << 2.036933
pc = 2*(1 - pt(tstatc, df = n -5 -1)) # = 0.7618377 > 5%
# so we cannot reject H0 since there is not enough evidence against the hypothesis H0: B2 = B3

# see line 56
fit2 = lm(Quality~Aroma+Flavor+Oakiness)
X2 = cbind(rep(1,n), Aroma, Flavor, Oakiness)
t(X2)%*%X2
vmatrix2 = solve(t(X2)%*%X2)
Bhat2 = solve(t(X2)%*%X2)%*%t(X2)%*%Quality
summary(fit2)
# Multiple R-squared:  0.457,	Adjusted R-squared:  0.4091
# we see that both models have roughly the same adjusted R-squared value, meaning that both multiple regression
# fit about the same.

# CI: first model
upper1 = Bhat[5,1] + qt(0.975, n-5-1)*se4
lower1 = Bhat[5,1] + qt(0.025, n-5-1)*se4
# (-0.3622388, 1.712737)
# regression coeff = 0.6752492 which is contained in the CI

# CI: second model
yhat2 = X2%*%Bhat2
r2 = Quality - yhat2
FlavorHat = Bhat2[3,1]
sig2Flavor = t(r2)%*%r2 / (n-3-1)
sigFlavor = sqrt(sig2Flavor)
seFlavor = sigFlavor*sqrt(vmatrix2[3,3])
upper2 = FlavorHat + qt(0.975, n-3-1)*seFlavor
lower2 = FlavorHat + qt(0.025, n-3-1)*seFlavor
# (0.1362725, 1.923218)
# regression coeff = 1.0297455 which is contained in the CI

# since there are less covariates in the second model, the covariate for flavour is higher

estPinotNoir = Bhat2[1,1] + Bhat2[2,1]*6 + Bhat2[3,1]*7 + Bhat2[4,1]*3
# = 16.79661







