# In France, a study was conducted to investigate potential risk factors for ectopic pregnancy.
# Of the 279 women who had experience ectopic pregnancy, 28 had suffered from pelvic inflammatory disease.  
# Of the 279 women who had not, 6 had suffered from the pelvic inflammatory disease.
# Using logistic regression model to investigate the effect of pelvic inflammatory diseaseon the ectopic pregnancy.

data = rbind(c(1,28, 251, 279), c(0,6, 273, 279))
colnames(data) = c('EctopicPregnancy', 'Disease (y)', 'No Disease (m-y)', 'm')
data.table = as.data.frame(data)
data.table$resp = cbind(data.table[,2],data.table[,3])

model1 = glm(resp ~ EctopicPregnancy, family=binomial(link=logit),data=data.table)
summary(model1)

# B1 is the log odds ratio of pelvic inflamatory disease to women who experienced ectopic pregnancy
# B1hat = 1.6245

seB1 = sqrt(diag(summary(model1)$cov.unscaled))[2]
oddsratio = exp(summary(model1)$coefficients[2]) # = 5.0756972
CIlower = exp(summary(model1)$coefficients[2] - 2.576*seB1)
CIupper = exp(summary(model1)$coefficients[2] + 2.576*seB1) # 99% CI for odds ratio (1.558799,16.52728)


# A study was carried out to investigate the effect of a new drug in reducing operative mortality
# following major abdominal surgery. Patients were assigned to treatment or control groups within each of 
# three categories of surgical risk: high, medium and low. The results were as follows:
treated = c(1,1,1,0,0,0)
medium  = c(0,1,0,0,1,0)
high    = c(0,0,1,0,0,1)
dead    = c(2,3,2,3,7,6)
survived= c(10,10,8,12,6,3)

data2   = cbind(treated,medium,high,dead,survived)
data2.table = as.data.frame(data2)
data2.table$resp = cbind(data2.table[,4],data2.table[,5])

# fit  the  logistic  regression  model  with  treatment  status  andsurgical risk

model2 = glm(resp ~ treated + medium + high, family=binomial(link=logit), data=data2.table)
summary(model2)

seB1.2 = sqrt(diag(summary(model2)$cov.unscaled))[2]
oddsratio.2 = exp(summary(model2)$coefficients[2])
CIlower.2 = exp(summary(model2)$coefficients[2] - 1.96*seB1.2)
CIupper.2 = exp(summary(model2)$coefficients[2] + 1.96*seB1.2)

prob_death_med = (exp(summary(model2)$coefficients[1]+summary(model2)$coefficients[3]))/
                  (1+exp(summary(model2)$coefficients[1]+summary(model2)$coefficients[3]))
c = c(1,0,1,0)
inv_info = vcov(model2)
se_prob_death_med = sqrt(t(c)%*%inv_info%*%c)

#CI for medium risk in control group
CIlower.2d = summary(model2)$coefficients[1]+summary(model2)$coefficients[3] -
              1.96*se_prob_death_med
CIupper.2d = summary(model2)$coefficients[1]+summary(model2)$coefficients[3] +
              1.96*se_prob_death_med
CIlower.2d = exp(CIlower.2d)/(1+exp(CIlower.2d)) #expit function
CIupper.2d = exp(CIupper.2d)/(1+exp(CIupper.2d))

prob_death_low = (exp(summary(model2)$coefficients[1]))/(1+exp(summary(model2)$coefficients[1]))

model2.3 = glm(resp ~ treated, family=binomial(link=logit), data=data2.table)
summary(model2.3)
model2.3$deviance - model2$deviance # = 4.726363
1-pchisq(model2.3$deviance - model2$deviance,4-2)


# The following table gives the data collected from a study of the analgesic effect of treatments
# on patients with neuralgia.  One test treatment and a placebo are compared. The response
# variable is given in the last column and indicates whether the patient reported pain or not
# after the treatment. Researchers also recorded the age, sex and the duration of the complaint
# of pain (in months) before treatment began.
# Fit a simple logistic regression model to investigate the effect of treatment on the pain.
data3 = rbind(c(1, 76, 1, 36, 0),c(1, 52, 1, 22, 0),c(0, 80, 0, 33, 1),c(1, 77, 1, 33, 1),
              c(1, 73, 0, 17, 1),c(0, 82, 0, 84, 1),c(1, 71, 1, 24, 1),c(0, 78, 0, 96, 1),
              c(1, 83, 0, 61, 0),c(1, 75, 0, 60, 0),c(0, 62, 1, 8, 1) ,c(0, 74, 0, 35, 1),
              c(1, 78, 0, 3, 0) ,c(1, 70, 0, 27, 0),c(0, 72, 1, 60, 1),c(1, 71, 0, 8, 0),
              c(0, 74, 0, 5, 1) ,c(0, 81, 0, 26, 0)) # treatment(Y=1, N=0), pain(Y=0, N=1)
colnames(data3) = c('Treatment','Age','Sex','Duration','Pain')
data3.table = as.data.frame(data3)

# Treatment
model3 = glm(Pain ~ Treatment, family=binomial(link=logit),data=data3.table)
summary(model3)
seB1.3 = sqrt(diag(summary(model3)$cov.unscaled))[2]
oddsratio3 = exp(summary(model3)$coefficients[2]) # = 0.06122449
CIlower = exp(summary(model3)$coefficients[2] - 1.96*seB1.3)
CIupper = exp(summary(model3)$coefficients[2] + 1.96*seB1.3) # 95% CI for odds ratio (0.0050562, 0.7413548)
# p-value = 0.0281 < 0.05 so we reject H0: B1 = 0, i.e we conclude there is an association between treatment and pain
# CI does not contain 1, so we conclude there is significant association between pain and treatment

# Age
model3 = glm(Pain ~ Age, family=binomial(link=logit),data=data3.table)
summary(model3)
seB1.3 = sqrt(diag(summary(model3)$cov.unscaled))[2]
oddsratio3 = exp(summary(model3)$coefficients[2]) # = 1.020202
CIlower = exp(summary(model3)$coefficients[2] - 1.96*seB1.3)
CIupper = exp(summary(model3)$coefficients[2] + 1.96*seB1.3) # 95% CI for odds ratio (0.896961, 1.160377)
# p-value = 0.761 >> 0.05 so there is no evidence to reject null hypothesis
# CI contains 1, so we conclude there is not a significant association between pain and age

# Sex
model3 = glm(Pain ~ Sex, family=binomial(link=logit),data=data3.table)
summary(model3)
seB1.3 = sqrt(diag(summary(model3)$cov.unscaled))[2]
oddsratio3 = exp(summary(model3)$coefficients[2]) # = 2
CIlower = exp(summary(model3)$coefficients[2] - 1.96*seB1.3)
CIupper = exp(summary(model3)$coefficients[2] + 1.96*seB1.3) # 95% CI for odds ratio (0.260049, 15.38172)
# p-value = 0.505 >> 0.05 so there is no evidence to reject null hypothesis
# CI contains 1, so we conclude there is not a significant association between pain and sex

# Duration
model3 = glm(Pain ~ Duration, family=binomial(link=logit),data=data3.table)
summary(model3)
seB1.3 = sqrt(diag(summary(model3)$cov.unscaled))[2]
oddsratio3 = exp(summary(model3)$coefficients[2]) # = 1.014223
CIlower = exp(summary(model3)$coefficients[2] - 1.96*seB1.3)
CIupper = exp(summary(model3)$coefficients[2] + 1.96*seB1.3) # 95% CI for odds ratio (0.9763763, 1.053536)
# CI contains 1, so we conclude there is not a significant association between pain and duration

# Now with a multiple logistic model including the main effects of all four explanatory
# variables, using backward elimination strategy to find the simplest model that adequately
# describes the variation in the response.
model3b = glm(Pain ~ Treatment+Age+Sex+Duration, family=binomial(link=logit),data=data3.table)
# starting with the full model with all 4 covariates, we will use the Likelihood Ratio Test to 
# find the simplest model that adequately describes the variation in the response. We do this by
# analyzing the p-values for each covariate. 
# Null Hypothesis: constrained model is adequate 
# Alternative Hypothesis: constrained model is not adequate
# we reject H0 if the p-value < alpha = 0.05
drop1(model3b, test = 'LRT')
# drop Duration first because its p-value=0.960913>>0.05
model3b = glm(Pain ~ Treatment+Age+Sex, family=binomial(link=logit),data=data3.table)
drop1(model3b, test = 'LRT')
# drop Age because its p-value=0.667794>>0.05
model3b = glm(Pain ~ Treatment+Sex, family=binomial(link=logit),data=data3.table)
drop1(model3b, test = 'LRT')
# drop Sex because its p-value=0.186315>0.05
model3b = glm(Pain ~ Treatment, family=binomial(link=logit),data=data3.table)
drop1(model3b, test = 'LRT')
# Stop here because p-value for Treatment = 0.01088 and thus, there we reject the null hypothesis 
# of Treatment being insignificant. This is the final simplest model.
