# Ashford and Sowden (1970) conducted a study on the health of British coal miners who were
# smokers but did not show radiological signs of a lung disorder called pneumoconiosis. A simple
# random sample of employed miners were assessed to determine whether they became breathless or 
# coughed upon mild exertion. The resulting data are displayed in the following, stratified according 
# to five year age intervals.
freq = c(1841,1654,1863,2357,1778,1712,1324,967,526,
         95,105,177,257,273,324,245,225,132,
         7,9,19,48,54,88,117,152,106,
         9,23,54,121,169,269,404,406,372)

names = list(A=c("20-24","25-29","30-34","35-39",
                 "40-44","45-49","50-54","55-59","60-64"),
             C=c("No","Yes"),
             B=c("No","Yes"))

health.3D = array(freq,c(9,2,2),dimnames=names)

library(plyr)
health = count(as.table(health.3D))
health = health[,1:4]
names(health)=c("A","C","B","Y")

# Fit the above log-linear models to these data

# Model 1: (BAC)
model1 = glm (Y~B*A*C, family = poisson, data=health)
model1$df.residual
model1$deviance

# Model 2: (BA, BC, AC)
model2 = glm(Y~B*A + B*C + A*C, family = poisson, data = health)
model2$df.residual
model2$deviance
1-pchisq(model2$deviance-model1$deviance,
         model2$df.residual-model1$df.residual)
# p-value = 0.0007994639 < 0.05, so we reject the null hypothesis that the fit of 
# model 2 is adequate as compared to model 1
# We therefore select Model 1: (BAC) as our final model
# Model 1 implies the saturated model is the most appropriate, which means the log linear
# model will provide a perfect fit to the data since it is saturated.
# There is no simpler representation of the relationship between age, breathlessness, and coughing.

summary(model1)

# Age 20-24 (A=1), Age 40-44 (A=5) Coughing (C=2), Not Coughing (C=1), Breathless (B=2), Not Breathless (B=1)

# log odds ratio of coughing among coal miners aged 20-24 who became breathless compared to those were not breathless
# logOR = u(BC,22) => OR = exp(u(BC,22))
exp(model1$coefficients[20])

# log odds ratio of coughing among coal miners aged 40-44 who became breathless compared to those were not breathless
# logOR = u(BC,22) + u(BAC,252) => OR = exp(u(BC,22) + u(BAC,252))
exp(model1$coefficients[20]+model1$coefficients[32])

exp(model1$coefficients[20])/exp(model1$coefficients[20]+model1$coefficients[32])-1
# we see the p-value for u(BAC,252) is 0.71097 >> 0.05, therefore there is no evidence to reject the null
# hypothesis that this covariate is insignificant. This means the odds ratio are not significantly different.

############################################################################################################################

# Persons with cystic fibrosis are susceptible to an accumulation of mucus in the lungs, which
# leads to pulmonary exacerbations and deterioration of lung function. In a randomized clinical trial, 
# a purified recombinant form of the human enzyme DNase I, called rhDNase, was administered daily to patients 
# in an rhDNase treatment group and the remaining patients were administered a placebo; patients and their
# physicians did not know which treatment(rhDNase or placebo) they were receiving. There were ultimately 324 
# and 321 subjects in the placebo and rhDNase groups, respectively. Most subjects were followed for roughly 
# 169 days,and the occurrences of exacerbations over the study period were recorded for each.

# Fit a poisson regression model to model the treatment effect on the response count controlling for the fev.

rhDNase.dat <- read.table("rhDNase.txt",header=T)
rhDNase.dat$trtf <- factor(rhDNase.dat$trt)
rhDNase.dat$trtft <- C(rhDNase.dat$trtf, treatment)
model0 <- glm(count ~ trtft + fev + log(time), family=poisson, data=rhDNase.dat)
summary(model0)
# The offset term log(time) explains the variation in the count due to different durations of observations

model1 <- glm(count ~ trtft + fev + offset(log(time)), family=poisson, data=rhDNase.dat)
summary(model1)

# Exponentiated coefficients of trt and fev correspond to the relative risk of counts.
# For trt, the RR is comparing the number of exacerbations experienced during the study for patients 
# treated with rhDNase vs patients with the placebo.
# For fev, the RR is comparing the number of exacerbations experienced during the study for patients
# with different baseline lung function.

RR.trt <- exp(model1$coefficients[2])
RR.trt 
RR.fev <- exp(model1$coefficients[3])
RR.fev
se.trt <- sqrt(diag(vcov(model1)))[2]
se.fev <- sqrt(diag(vcov(model1)))[3]

CI.upper.trt <- model1$coefficients[2] + 1.96*se.trt
CI.lower.trt <- model1$coefficients[2] - 1.96*se.trt
CI.trt <- c(exp(CI.lower.trt), exp(CI.upper.trt))
CI.trt

CI.upper.fev <- model1$coefficients[3] + 1.96*se.fev
CI.lower.fev <- model1$coefficients[3] - 1.96*se.fev
CI.fev <- c(exp(CI.lower.fev), exp(CI.upper.fev))
CI.fev

rhDNase.dat$rdeviance1 <- residuals.glm(model1, type="deviance")
plot(model1$fitted.values, rhDNase.dat$rdeviance1, ylim=c(-4,4), xlab="FITTED VALUES",ylab="DEVIANCE RESIDUALS", main="POISSON MODEL")
abline(h=-2)
abline(h=2)

# The residuals do not seem to be randomly scattered between (-2, 2). So this may indicated that the model
# is not an appropriate fit for the data. There may be some evidence of overdispersion in the data.
# See plot below

mean(rhDNase.dat$count)
var(rhDNase.dat$count)
mean(rhDNase.dat$count[rhDNase.dat$trt==1])
var(rhDNase.dat$count[rhDNase.dat$trt==1])
mean(rhDNase.dat$count[rhDNase.dat$trt==0])
var(rhDNase.dat$count[rhDNase.dat$trt==0])
mean(rhDNase.dat$fev)
var(rhDNase.dat$fev)

model1$deviance
model1$df.residual
# D (1468.526) >> n-p (642). Therefore overdispersion exists!

phi <- model1$deviance / model1$df.residual
phi
adj.se.trt <- sqrt(phi)*se.trt
adj.se.fev <- sqrt(phi)*se.fev

CI.upper.trt.adj <- model1$coefficients[2] + 1.96*adj.se.trt
CI.lower.trt.adj <- model1$coefficients[2] - 1.96*adj.se.trt
CI.trt.adj <- c(exp(CI.lower.trt.adj), exp(CI.upper.trt.adj))
CI.trt.adj

CI.upper.fev.adj <- model1$coefficients[3] + 1.96*adj.se.fev
CI.lower.fev.adj <- model1$coefficients[3] - 1.96*adj.se.fev
CI.fev.adj <- c(exp(CI.lower.fev.adj), exp(CI.upper.fev.adj))
CI.fev.adj

# We see that using the Ad Hoc Method doesn't change the statistical significance of the estimates.
# It does adjust the standard errors by a factor of sqrt(phi) to account for overdispersion.
# This widens the confidence intervals since the factor of sqrt(phi) > 1

library(MASS)
count <- rhDNase.dat$count
trtft <- rhDNase.dat$trt
fev <- rhDNase.dat$fev
time <- rhDNase.dat$time
model2 <- glm.nb(count ~ trtft + fev + offset(log(time)), link=log, init.theta=1.00)
summary(model2)

# We compare the p-values. We see that the corresponding p-values for trt and fev are 0.0522 and 4.59e-10 respectively.
# Thus, the covariate effects are not the same in the Negative Binomial model compared to the Poisson model. 
# Only the trt covariate is statisically significant for the Negative Binomial model, 
# whereas the Poisson model had no covariates that were significant.
# The magnitudes and directions of the estimates of trt and fev are similar under both models. 

rhDNase.dat$rdeviance2 <- residuals.glm(model2, type="deviance")
par(mfrow=c(1,2))
plot(model1$fitted.values, rhDNase.dat$rdeviance1, ylim=c(-4,4), xlab="FITTED VALUES",ylab="DEVIANCE RESIDUALS", main="POISSON MODEL")
abline(h=-2)
abline(h=2)

plot(model2$fitted.values, rhDNase.dat$rdeviance2, ylim=c(-4,4), xlab="FITTED VALUES",ylab="DEVIANCE RESIDUALS", main="NEG BIN MODEL")
abline(h=-2)
abline(h=2)

## The Negative Binomial model fits a little bit better as the residuals are more scattered inside (-2,2) range.

# The probability of zero under Negative Binomial is (theta/(mu + theta))^theta
theta <- model2$theta
mu <- exp(predict(model2))
zero <- (theta/(mu + theta))^theta
expected_proportion <- mean(zero)
c(expected_proportion, mean(rhDNase.dat$count==0))

# It is predicted that 58.63% will have zero exacerbations, compared to the observed proportion of 62.02%.

##############################################################################################################################

# The data in the following table arose from a study designed to examine the effect of a treatment 
# on the occurrence of premature ventricular contractions (PVCs) in patients with irregular heart beats.  
# Patients were assessed before they were given a test drug and afterward, and during each assessment the 
# number of PVCs occuring during a 1 minute ECG test was recorded.
data = cbind(c(1,2,3,4,5,6,7,8,9,10,11,12),
             c(6,9,17,22,7,5,5,14,9,7,9,51),
             c(5,2,0,0,2,1,0,0,0,0,13,0),
             c(11,11,17,22,9,6,5,14,9,7,22,51))
data = as.data.frame(data)
names(data) = c("id","pre","post","m")
data$resp <- cbind(data$post, data$m - data$post)

model1 <- glm(resp ~ 1, family = binomial(link = logit), data = data)
summary(model1)

# Since the p-value is <2e-16 << 0.05, we reject the null hypothesis. This means we conclude that
# the log (odds ratio) is not equal to zero, which is the same as saying the odds ratio
# is not equal to 1. Thus, the drug does have an effect on the occurence of premature ventricular contractions.