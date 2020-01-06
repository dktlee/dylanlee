library(survival)
library(survminer)
leukemia.data = read.csv("LeukemiaData.csv")
attach(leukemia.data)
surv.object = Surv(time = leukemia.data$weeks, event = leukemia.data$relapse)
surv.object
fit = survfit(surv.object ~ group, data = leukemia.data,conf.int=0.8, conf.type="log-log")

summary(fit)
print(fit)
# from the summary output, we see that the KM estimate for S(10) for the control group is 0.3810
# and the KM estimate for S(10) for the treated group is 0.753
print(paste("The KM estimate for S(10) for the control group is", summary(fit)$surv[6]))
print(paste("The KM estimate for S(10) for the treated group is", summary(fit)$surv[15]))

y_i.ctrl <- summary(fit)$time[1:12]
prob.ctrl <- summary(fit)$surv[1:12]
r_i.ctrl <- summary(fit)$n.risk[1:12]
s_i.ctrl <- summary(fit)$n.event[1:12]

y_i.trt <- summary(fit)$time[13:19]
prob.trt <- summary(fit)$surv[13:19]
r_i.trt <- summary(fit)$n.risk[13:19]
s_i.trt <- summary(fit)$n.event[13:19]

var.hat.factor.trt = cumsum(s_i.trt/r_i.trt/(r_i.trt-s_i.trt))
var.hat.factor.ctrl = cumsum(s_i.ctrl/r_i.ctrl/(r_i.ctrl-s_i.ctrl))

var.hat.trt = (prob.trt^2)*var.hat.factor.trt
var.hat.ctrl = (prob.ctrl^2)*var.hat.factor.ctrl

# Var_hat(S(10)) for treatment group
# the estimate of the variance of S(10) for the treated group with KM estimator is 0.009283256
var.hat.trt[3]

# 80% linear CI for S(10) for the treated group with KM is (0.629, 0.876)
summary(survfit(surv.object ~ group, data = leukemia.data, conf.int=0.8, conf.type="plain"))

# 80% log transformed CI for the treated group with KM is (0.603, 0.853)
summary(survfit(surv.object ~ group, data = leukemia.data, conf.int=0.8, conf.type="log-log"))

ctrl.exp.time = c(1,0,0,0,0,0,0,0,0,0,0,0)
trt.exp.time = c(6,0,0,0,0,0,0)

for (x in 1:11){
  ctrl.exp.time[x+1] = (y_i.ctrl[x+1]-y_i.ctrl[x])*prob.ctrl[x]
}
ctrl.exp.time = sum(ctrl.exp.time)
ctrl.exp.time
# the expected time until failure for the control group with KM estimator is 8.666667
for (x in 1:6){
  trt.exp.time[x+1] = (y_i.trt[x+1]-y_i.trt[x])*prob.trt[x]
}
trt.exp.time = sum(trt.exp.time)
trt.exp.time
# the expected time until failure for the treatment group with KM estimator is 17.90924
