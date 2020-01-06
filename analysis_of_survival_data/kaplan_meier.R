library(survival)
library(survminer)
leukemia.data = read.csv("LeukemiaData.csv")

surv.object = Surv(time = leukemia.data$weeks, event = leukemia.data$relapse)
fit = survfit(surv.object ~ group, data = leukemia.data)
summary(fit)
print(fit)
# from the summary output, we see that the KM estimate for S(15) for the control group is 0.1429
# and the KM estimate for S(15) for the treated group is 0.690
print(paste("The KM estimate for S(15) for the control group is", summary(fit)$surv[9]))
print(paste("The KM estimate for S(15) for the treated group is", summary(fit)$surv[16]))
ggsurvplot(fit, data = leukemia.data)

yi = summary(fit)$time
ri = summary(fit)$n.risk
si = summary(fit)$n.event
control.NA.result = c(si[1]/ri[1])
treated.NA.result = c(si[13]/ri[13])

for (x in c(2,3,4,5,6,7,8,9,10,11,12))
  control.NA.result = c(control.NA.result,control.NA.result[x-1] + si[x]/ri[x])
control.NA.result = exp(-control.NA.result)

for (x in c(2,3,4,5,6,7))
  treated.NA.result = c(treated.NA.result,treated.NA.result[x-1] + si[x+12]/ri[x+12])
treated.NA.result = exp(-treated.NA.result)
# we get that the NA estimate for S(15) for the control group is 0.18381 and for the treated 
# group is 0.703505
print(paste("The NA estimate for S(15) for the control group is", control.NA.result[9]))
print(paste("The NA estimate for S(15) for the treated group is", treated.NA.result[4]))