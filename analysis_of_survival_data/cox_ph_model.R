library(survival)
library(survminer)
rossi.data = read.csv("Rossi.csv")
attach(rossi.data)

cox.model <- coxph(Surv(week, arrest) ~ relevel(fin, ref = "no") + age + relevel(race, ref = "other") + relevel(wexp, ref = "no")
                   + relevel(mar, ref = "not married") + relevel(paro, ref = "no") + prio + factor(educ),
                   data = rossi.data)
cox.model
summary(cox.model)
