# In a biomedical study of an immuno-activating ability of two agents TNF (tumour necrosis
# factor) and IFN (interferon), to induce cell differentiation, the number of cells that exhibit
# markers of differentiation after exposure to TNF and/or IFN was recorded. At each of
# the 16 dose combinations of TNF/IFN, 200 cells were examined.
data = cbind(c(0,0,0,0,1,1,1,1,10,10,10,10,100,100,100,100),
             c(0,4,20,100,0,4,20,100,0,4,20,100,0,4,20,100),
             c(11,18,20,39,22,38,52,69,31,68,69,128,102,171,180,193))
data = cbind(data, 200 - data[,3])

# Let xi1 be the log-concentration (the dose) of TNF (U/ml) which measures the intensity of TNF on the number of
# differentiating cells for the ith group.
# Let xi2 be the log-concentration (the dose) of IFN (U/ml) which meausres the intensity of IFN on the number of 
# differentiating cells for the ith group.
data = cbind(data, log(data[,1]), log(data[,2]))
data = cbind(data, data[,1]*data[,2])
colnames(data) = c('Dose_of_TNF', 'Dose_of_IFN', '# Differentiating Cells (y)',
                   '# No Differentiating Cells (200-y)','xi1', 'xi2', 'xi3')
data.table = as.data.frame(data)
data.table$resp = cbind(data.table[,3],data.table[,4])

# Model1: Main effects
model1 = glm(resp ~ Dose_of_TNF + Dose_of_IFN, family=binomial(link=logit),data=data.table)
summary(model1)

data.table$rdeviance <- residuals.glm(model1,type="deviance")
plot(model1$fitted.values,data.table$rdeviance,ylim=c(-4,4),xlab="FITTED VALUES",ylab="DEVIANCE RESIDUALS")
abline(h=-2)
abline(h= 2)

# here we record deviance residuals in rd1
rd1 <- residuals.glm(model1,"deviance")
fv1 <- model1$fitted.values
# plotting the deviance residuals by dose and by fitted values
pdf("cells-residuals.pdf")
par(mfrow=c(3,2))
plot(data.table$Dose_of_TNF,rd1,ylim=c(-5,5), xlab="DOSE",ylab="DEVIANCE RESIDUALS")
abline(h=-2) ; abline(h= 2); title("Model 1 - logit link")
plot(fv1,rd1,ylim=c(-5,5), xlab="FITTED VALUE",ylab="DEVIANCE RESIDUALS")
abline(h=-2) ; abline(h= 2); title("Model 1 - logit link")

# Model2: Dose_of_TNF only
model2 = glm(resp ~ Dose_of_TNF, family=binomial(link=logit),data=data.table)
summary(model2)
1-pchisq(model2$deviance-model1$deviance, model2$df.residual-model1$df.residual)
# p-value = 0, so we reject H0, and conclude that Dose of IFN has a significant effect on # of differentiating cells

# Model3: Dose_of_IFN only
model3 = glm(resp ~ Dose_of_TNF, family=binomial(link=logit),data=data.table)
summary(model3)
1-pchisq(model3$deviance-model1$deviance, model3$df.residual-model1$df.residual)
# p-value = 0, so we reject H0, and conclude that Dose of TFN has a significant effect on # of differentiating cells

# Model4: Main effects + interaction
model4 = glm(resp ~ Dose_of_TNF + Dose_of_IFN + xi3, family=binomial(link=logit),data=data.table)
summary(model4)
1-pchisq(model1$deviance-model4$deviance, model1$df.residual-model4$df.residual)
# p-value = 1.611279e-10, so we reject H0, and conclude the interaction term is significant
# Hence, Model4 with the interaction term is the most appropriate for describing the distribution of responses.

# B1 is the log-relative-rate of cells differentiating for cells exposed to TNF only. 
exp(model4$coefficients[2])
# So the rate of cells differentiating for cells exposed to TFN is 1.025527 times higher than cells not exposed to it

# B2 is the log-relative-rate of cells differentiating for cells exposed to IFN only. 
exp(model4$coefficients[3])
# So the rate of cells differentiating for cells exposed to IFN is 1.010733 times higher than cells not exposed to it

# B3 is the log-relative-rate of cells differentiating for cells exposed to both TFN and IFN.
exp(model4$coefficients[4])
# So the rate of cells differentiating for cells exposed to both TFN and IFN is 1.000384 times higher than cells 
# exposed to only TFN or only IFN


# Model1: Main effects
model1c = glm(resp ~ Dose_of_TNF + Dose_of_IFN, family=binomial(link=cloglog),data=data.table)
summary(model1c)
# Model2: Dose_of_TNF only
model2c = glm(resp ~ Dose_of_TNF, family=binomial(link=cloglog),data=data.table)
summary(model2c)
1-pchisq(model2c$deviance-model1c$deviance, model2c$df.residual-model1c$df.residual)
# p-value = 0, so we reject H0, and conclude that Dose of IFN has a significant effect on # of differentiating cells
# Model3: Dose_of_IFN only
model3c = glm(resp ~ Dose_of_TNF, family=binomial(link=cloglog),data=data.table)
summary(model3c)
1-pchisq(model3c$deviance-model1c$deviance, model3c$df.residual-model1c$df.residual)
# p-value = 0, so we reject H0, and conclude that Dose of TFN has a significant effect on # of differentiating cells
# Model4: Main effects + interaction
model4c = glm(resp ~ Dose_of_TNF + Dose_of_IFN + xi3, family=binomial(link=cloglog),data=data.table)
summary(model4c)
1-pchisq(model1c$deviance-model4c$deviance, model1c$df.residual-model4c$df.residual)
# p-value = 0.4429069, so there is no evidence to reject H0, and conclude the interaction term is not significant
# Hence, Model1 without the interaction term is the most appropriate for describing the distribution of responses using 
# the complementary log log link.

############################################################################################################################

# Car insurance claims data is saved in the data file “car.txt”. The data set gives numbers (N) of policyholders of 
# car insurance from an insurance company, and the number (C) of car insurance claims made in the third quarter 
# of 1973 by these policyholders. The data are cross-classified by three four-level factors:
# D, the district in which the policyholder lived, 
# G, the insurance group into which the car was placed, and
# A,the age of the policyholder.

car.data <- read.table("car.txt", header=T)

car.data$Df <- factor(car.data$D)
car.data$Dft <- C(car.data$Df, treatment)
car.data$Gf <- factor(car.data$G)
car.data$Gft <- C(car.data$Gf, treatment)
car.data$Af <- factor(car.data$A)
car.data$Aft <- C(car.data$Af, treatment)
car.data$Df2 <- factor(car.data$D)
car.data$Df2[car.data$Df2==3] <- 2
car.data$Dft2 <- C(car.data$Df2, treatment)

model1 <- glm(C ~ Dft + Gft + Aft + log(N), family=poisson, data=car.data)
summary(model1)
# We see the estimate of log(N) is 1.201696 with a  p-value < 2e-16. So we can conclude it is not significantly 
# different than 1, thus the data is consistent with a time-homogeneous model.

# A reasonable assumption on the distribution with respect to the number of claims is that we can assume the number of claims 
# follow a Time-Homogeneous Poisson Process. The number of claims does not change over time. 
# The log-link model must be used for regression.
# The number of policyholders should be used as the offest term because it attempts to explain the variant in the event 
# counts across subjects in a dterministic way.
# An expression for the main effects model is:
# N = b0 + b1*(D2) + b2*(D3) + b3*(D4) + b4*(G2) + b5*(G3) + b6*(G4) + b7*(A2) + b8*(A3) + b9*(A4) + log(N)

model1 <- glm(C ~ Dft + Gft + Aft + offset(log(N)), family=poisson, data=car.data)
summary(model1)
deviance1 <- model1$deviance
residual1 <- model1$df.residual

model2a <- glm(C ~ Gft + Aft + offset(log(N)), family=poisson, data=car.data)
summary(model2a)
deviance2a <- model2a$deviance
residual2a <- model2a$df.residual
p_value2a <- 1-pchisq(deviance2a - deviance1, residual2a - residual1)
p_value2a
# p-value = 0.003085734 < 0.05 and so there is evidence that D is significant.

model2b <- glm(C ~ Dft + Aft + offset(log(N)), family=poisson, data=car.data)
summary(model2b)
deviance2b <- model2b$deviance
residual2b <- model2b$df.residual
p_value2b <- 1-pchisq(deviance2b - deviance1, residual2b - residual1)
p_value2b
# p-value = 0 < 0.05 and so there is evidence that G is significant.

model2c <- glm(C ~ Dft + Gft + offset(log(N)), family=poisson, data=car.data)
summary(model2c)
deviance2c <- model2c$deviance
residual2c <- model2c$df.residual
p_value2c <- 1-pchisq(deviance2c - deviance1, residual2c - residual1)
p_value2c
# p-value = 0 < 0.05 and so there is evidence that A is significant

car.data$rdeviance <- residuals.glm(model1, type="deviance")
plot(model1$fitted.values, car.data$rdeviance, ylim=c(-4,4), xlab="Fitted Values", ylab="Deviance Residuals")
abline(h=-2)
abline(h=2)
# From the deviance test and plotting the fitted values against deviance residuals,
# we see that all the factors are significant and conclude that the full model is reasonable. 

b2 <- model1$coefficients[2]
b3 <- model1$coefficients[3]
b4 <- model1$coefficients[4]
V <- summary(model1)$cov.unscaled
C <- as.matrix(c(0,-1,1,0,0,0,0,0,0,0), ncol=1)
se.b2.b3 <- sqrt(t(C)%*%V%*%C)
z <- (b3 - b2) / se.b2.b3

# We notice that the p-values for  Dft2 and Dft3 are greater than 0.05 which means that these districts 
# are not significant on their own.
# H0: B2 - B3 = 0
p_value <- 2*(1-pnorm(z))
p_value
# p-value = 0.8153922, so there is no evidence to reject the null hypothesis. Hence these two districts
# can be combined. 

# H0: Model 3 is adequate (Districts 2 & 3 combined)
# H1: Model 1 is adequate and Model 3 is not adequate
model3 <- glm(C ~ Dft2 + Gft + Aft + offset(log(N)), family=poisson, data=car.data)
summary(model3)
deviance3 <- model3$deviance
residual3 <- model3$df.residual
p_value3 <- 1-pchisq(deviance3 - deviance1, residual3 - residual1)
p_value3
# p-value = 0.8154817 which means there is no evidence to reject the null hypothesis H0. Hence Model 3
# is sufficient - the model with district 2 & 3 combined. 

Xi <- as.matrix(c(1,1,0,0,0,1,0,0,0), ncol=1)
Vi <- summary(model3)$cov.unscaled
b0 <- model3$coefficients[1]
b1 <- model3$coefficients[2]
b6 <- model3$coefficients[6]
lambda <- b0 + b1 + b6
se.lambda <- sqrt(t(Xi)%*%Vi%*%Xi)
exp_claim <- exp(lambda)
upper <- lambda + 1.96*se.lambda
lower <- lambda - 1.96*se.lambda
CIu <- exp(upper)
CIl <- exp(lower)
CI <- c(CIl, CIu)
exp_claim
CI
# expected claims = 0.2929025
# 95% confidence interval = [0.2454222, 0.3495685]

############################################################################################################################

# Consider the following data on newly diagnosed lung cancer cases occurring in four Danish cities between 1968 and 1971.
# A Poisson model is often used to analyze data of this sort based on the expected number of cases 
# (recall the Poisson approximation for the binomial model). However, variable Pop(population size) suggests 
# a systematic (non-random) way in which the number of cases will vary from city to city and across age groups.  
# It is therefore preferable to model the disease rate and use the log of the population size as an offset term.  
# Let μ denote the mean number of newly diagnosed lung cancer patients 1968 -1971, x be a p×1 covariate vector and
# βbethe associated vector of regression coefficients, we specify a model as log(μ) =x′β+ log(Pop.)
# Fit a main effects model as above to the data

lung.data <- read.table("lung-cancer.txt", header=T)

# C = number of cases of lung cancer
# P = population size of the Danish cities 
# Ci = city for which data was sampled
#      F = Frederica, H = Horsens, K = Kolding, V = Vejile
# A = age group of the individuals
#      1 = 45-54, 2 = 55-59, 3 = 60-64, 4 = 65-69, 5 = 70-74, 6 = 70+

lung.data$Cif <- factor(lung.data$Ci)
lung.data$Cift <- C(lung.data$Cif, treatment)
lung.data$Af <- factor(lung.data$A)
lung.data$Aft <- C(lung.data$Af, treatment)

model1 <- glm(C ~ Cift + Aft + offset(log(P)), family=poisson, data=lung.data)
summary(model1)
deviance1 <- model1$deviance
residual1 <- model1$df.residual

Xi <- as.matric(c(1,0,1,0,1,0,0,0,0), ncol=1)
Vi <- summary(model1)$cov.unscaled
b0 <- model1$coefficients[1]
b3 <- model1$coefficients[3]
b5 <- model1$coefficients[5]
lambda <- b0 + b3 + b5
se.lambda <- sqrt(t(Xi)%*%Vi%*%Xi)
mean_cases <- exp(lambda)
upper <- lambda + 1.96*se.lambda
lower <- lambda - 1.96*se.lambda
# offset = 1050 which is the population  of the city of Kolding
CIu <- exp(upper) * 1050
CIl <- exp(lower) * 1050
CI <- c(CIl, CIu)
mean_cases
CI
# There are 0.0074272727 mean number of newly diagnosed lung cancer patients over the 3 year period in Kolding for the 55-59
# age group
# 95% confidence interval = [5.368624, 11.328547]

b5 <- model1$coefficients[5]
b9 <- model1$coefficients[9]
V <- summary(model1)$cov.unscaled
C <- as.matrix(c(0,0,0,0,-1,0,0,0,1), ncol=1)
se.b5.b9 <- sqrt(t(C)%*%V%*%C)
b9_b5 <- b9 - b5
RR <- exp(b9_b5)
upper <- b9_b5 + 1.96*se.b5.b9
lower <- b9_b5 - 1.96*se.b5.b9
CIu <- exp(upper)
CIl <- exp(lower)
CI <- c(CIl, CIu)
RR
CI

# The estimated relative change in the lung cancer rate for age groups 70+ against 55-59 is 1.375255.
# 95% confidence interval = [0.8391406, 2.2538860]

# H0: main effects with interaction term is adequate
# H1: model without interaction is adequate, interaction term is insignificant
model2 <- glm(C ~ Cift + Aft + Cift*Aft + offset(log(P)), family=poisson, data=lung.data)
summary(model2)
deviance2 <- model2$deviance
residual2 <- model2$df.residual
p_value <- 1-pchisq(deviance1 - deviance2, residual1-residual2)
p_value

# The p_value = 0.07509017 > 0.05. Thus there is some evidence to reject H0: interaction model is adequate.
# We conclude that the main effects model without the interaction term is adequate.

b5 <- model1$coefficients[5]
b9 <- model1$coefficients[9]
V <- summary(model1)$cov.unscaled
C <- as.matrix(c(0,0,0,0,-1,0,0,0,1), ncol=1)
se.b5.b9 <- sqrt(t(C)%*%V%*%C)
b9_b5 <- b9 - b5
RR <- exp(b9_b5)
# Since the model was for 3 years, we take the population and divide by 3 to get an estimate for 1 year period
ti <- 100000/3
expected_cases <- RR * ti
expected_cases
# The estimated number of cases per 100,000 person-years in 1970 for the city of Kolding,
# for the ages 55-59 have an expected 45,841.84 number of cases. 
# (This is my assumption of 3 years) To be complete just in case:
# Counting 1968, 1969, 1970, 1971 as individuals years, it is a 4 year study. 
# if we let ti = 100000/4, we get that the expected number of cases is 34,381.38

############################################################################################################################

# Consider a study of relationship between smoking and lung cancer. A group of subjects are cross 
# classified by their smoking habit and lung cancer status.
# Using the Binomial regression methods to analyze the data, estimate the odds ratio of getting lung cancer 
# for smokers vs non-smokers and give a 95% confidence interval for your estimate.

smoker.data <- read.table("smoking.txt", header=T)
smoker.data$resp <- cbind(smoker.data$LC, smoker.data$m - smoker.data$LC)
model1 <- glm(resp ~ Sm, family=binomial(link=logit), data=smoker.data)
summary(model1)
b1 <- model1$coefficients[2]
V <- summary(model1)$cov.unscaled
C <- as.matrix(c(0,1), ncol=1)
se.b1 <- sqrt(t(C)%*%V%*%C)
OR <- exp(b1)
upper <- b1 + 1.96*se.b1
lower <- b1 - 1.96*se.b1
CIu <- exp(upper)
CIl <- exp(lower)
CI <- c(CIl, CIu)
OR
CI
# The estimated odds ratio of getting lung cancer for smokers vs. non-smokers is  2.973773.
# 95% confidence interval = [1.786720, 4.959474]

smoker.data$Smf <- factor(smoker.data$Sm)
smoker.data$Smft <- C(smoker.data$Smf, treatment)
model2 <- glm(LC ~ Smft, family=poisson, data=smoker.data)
summary(model2, corr=T)
smoker.data$fitted.values <- model2$fitted.values
smoker.data$rdeviance <- residuals.glm(model2, type="deviance")
smoker.data

