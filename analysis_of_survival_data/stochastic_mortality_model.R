install.packages("demography")
library(demography)

install.packages("StMoMo")
library(StMoMo)

install.packages("lifecontingencies")
library(lifecontingencies)

CAN<-hmd.mx(country="CAN", username="*****",
            password="*****", label="Canada")

Can.data <-StMoMoData(data=CAN, series = "female",type="central")

#Fit Lee-Carter model on ages 50 to 100
LC <- lc(link = "log")
LCfit <- fit(LC, data = Can.data, ages.fit = 50:100)
plot(LCfit, nCol = 3)

#Fit CBD model on ages 50 to 100
#first convert central exposures to intital exposures
can_int <- central2initial(Can.data)

CBD <- cbd(link = "logit")
CBDfit <- fit(CBD, data = can_int, ages.fit = 50:100)
plot(CBDfit, parametricbx = FALSE)

#Fit M7 model on ages 50 to 100
M7 <- m7(link = "logit")
M7fit <- fit(M7, data = can_int, ages.fit = 50:100)
plot(M7fit, parametricbx = FALSE)

#Check BIC for each model
BIC(LCfit)
BIC(CBDfit)
BIC(M7fit)

#Examine standarized residuals for each model
LCres <- residuals(LCfit, scale = FALSE)
plot(LCres, type = "scatter")
plot(LCres, type = "signplot")

CBDres <- residuals(CBDfit, scale = FALSE)
plot(CBDres, type = "scatter")
plot(CBDres, type = "signplot")

M7res <- residuals(M7fit, scale = FALSE)
plot(M7res, type = "scatter")
plot(M7res, type = "signplot")

#Check parameter robustness by fitting with years 1941 to 2016
#and compare versus 1921 - 2016
LCfit2 <- fit(LC, data = Can.data, ages.fit = 50:100, years.fit = 1941:2016)

plot(LCfit$ages, LCfit$bx, xlab = "age", ylab = "bx", main = "LC")
lines(LCfit2$ages, LCfit2$bx, lty = 2)

plot(LCfit$ages, LCfit$ax, xlab = "age", ylab = "ax", main = "LC")
lines(LCfit2$ages, LCfit2$ax, lty = 2)

plot(LCfit$years, LCfit$kt[1,], xlab = "year", ylab = "kt", main = "LC")
lines(LCfit2$years, LCfit2$kt[1,], lty = 2)

CBDfit2 <- fit(CBD, data = can_int, ages.fit = 50:100, years.fit = 1941:2016)

plot(CBDfit$years, CBDfit$kt[1,], xlab = "year", ylab = "kt(1)", main = "CBD")
lines(CBDfit2$years, CBDfit2$kt[1,], lty = 2)

plot(CBDfit$years, CBDfit$kt[2,], xlab = "year", ylab = "kt(2)", main = "CBD")
lines(CBDfit2$years, CBDfit2$kt[2,], lty = 2)

M7fit2 <- fit(M7, data = can_int, ages.fit = 50:100, years.fit = 1941:2016)

plot(M7fit$years, M7fit$kt[1,], xlab = "year", ylab = "kt(1)", main = "M7")
lines(M7fit2$years, M7fit2$kt[1,], lty = 2)

plot(M7fit$years, M7fit$kt[2,], xlab = "year", ylab = "kt(2)", main = "M7")
lines(M7fit2$years, M7fit2$kt[2,], lty = 2)

plot(M7fit$years, M7fit$kt[3,], xlab = "year", ylab = "kt(3)", main = "M7")
lines(M7fit2$years, M7fit2$kt[3,], lty = 2)

plot(M7fit$cohorts, M7fit$gc, xlab = "cohort", ylab = "gamma", main = "M7")
lines(M7fit2$cohorts, M7fit2$gc, lty = 2)

#Simulations
LCsim <- simulate(LCfit, nsim = 1000, h = 50, jumpchoice = "actual")
CBDsim <- simulate(CBDfit, nsim = 1000, h = 50, jumpchoice = "actual")
M7sim <- simulate(M7fit, nsim = 1000, h = 50, jumpchoice = "actual",
                  gc.order = c(2, 0, 0))

LC_mx_fit <- fitted(LCfit, type = "rates") #gives central death rates
CBD_qx_fit <- fitted(CBDfit, type = "rates") #gives 1 year death probabilities
CBD_mx_fit <- qx2mx(CBD_qx_fit) #default assumes UDD, CBD_qx/(1-0.5*CBD_qx) is equivalent
M7_mx_fit <- qx2mx(fitted(M7fit, type = "rates"))

CBD_mx_sim_65 <- apply(CBDsim$rates["65", ,],2,qx2mx)
M7_mx_sim_65 <- apply(M7sim$rates["65", ,],2,qx2mx)

#Plot 20 simulated sets of central death rates for age 65
plot(LCfit$years, LC_mx_fit["65",], xlim = range(LCfit$years, LCsim$years),
     ylim = range(fitted(LCfit, type = "rates")["65",], LCsim$rates["65", , 1:20]), 
     type = "l",xlab = "year", ylab = " central death rate", 
     main = "L-C Simulation: Central Death Rates at Age 65")
matlines(LCsim$years, LCsim$rates["65", , 1:20], type = "l", lty = 1)

plot(CBDfit$years, CBD_mx_fit["65",], xlim = range(CBDfit$years, CBDsim$years),
     ylim = range(CBD_mx_fit["65",], CBD_mx_sim_65[, 1:20]), 
     type = "l",xlab = "year", ylab = " central death rate", 
     main = "CBD Simulation: Central Death Rates at Age 65")
matlines(CBDsim$years, CBD_mx_sim_65[, 1:20], type = "l", lty = 1)

plot(M7fit$years, M7_mx_fit["65",], xlim = range(M7fit$years, M7sim$years),
     ylim = range(M7_mx_fit["65",],M7_mx_sim_65[, 1:20]), 
     type = "l",xlab = "year", ylab = " central death rate", 
     main = "M7 Simulation: Central Death Rates at Age 65")
matlines(M7sim$years, M7_mx_sim_65[, 1:20], type = "l", lty = 1)

#Extract 1952 cohort for life aged 65 in year 2017
LC_1952_sim <- apply(extractCohort(LCsim$rates, cohort=1952),2,mx2qx)
CBD_1952_sim <- extractCohort(CBDsim$rates, cohort=1952)
M7_1952_sim <- extractCohort(M7sim$rates, cohort=1952)

#Find annuity EPV for each simulation
LC_EPV <- rep(NA,1000) #vector to store EPVs
for(i in 1:1000)
{lti<-probs2lifetable(probs=LC_1952_sim[,i],type="qx")
ati<-new("actuarialtable",x=lti@x,
            lx=lti@lx,interest=0.05)
LC_EPV[i] <- axn(actuarialtable = ati,x=0)}

mean(LC_EPV)

CBD_EPV <- rep(NA,1000)
for(i in 1:1000)
{lti<-probs2lifetable(probs=CBD_1952_sim[,i],type="qx")
ati<-new("actuarialtable",x=lti@x,
         lx=lti@lx,interest=0.05)
CBD_EPV[i] <- axn(actuarialtable = ati,x=0)}

mean(CBD_EPV)

M7_EPV <- rep(NA,1000)
for(i in 1:1000)
{lti<-probs2lifetable(probs=M7_1952_sim[,i],type="qx")
ati<-new("actuarialtable",x=lti@x,
         lx=lti@lx,interest=0.05)
M7_EPV[i] <- axn(actuarialtable = ati,x=0)}

mean(M7_EPV)
