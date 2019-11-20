##########################################################################################################
                            ### Data Exploration & Cleanup ###


trucking <- read.csv("trucking.csv", header=T)

# add the factor version of PRODUCT to the dataset "trucking" since it has "classification" 
# and is therefore a categorical variable.
trucking$PRODUCT.f <- as.factor(trucking$PRODUCT) 

# drop the integer version of PRODUCT from the old dataset
trucking$PRODUCT <- NULL 

# Attach for convenience. Remember to detach first if the dataset is already attached!
attach(trucking)

summary(trucking) 
# No immediately suspicious outliers from the 5 number summary. The max and min of 
# PRICEPTM is high and low, but they are not isolated points, 
# instead there are a bunch that are very cheap or expensive
# These points respectively correspond to efficient and inefficient use of trucks 
# (distance and weight), so they make sense. 
# We do a more in-depth outlier search once we have some models.


                                      ### Pairwise Plots ###

pairs(trucking[,-1], pch=".", cex=3, col="gray30", main="Pairwsie Plot of Dataset")
# Again, no immediately obvious outliers. 
# There appears to be a collinearity between WEIGHT and PCTLOAD. 
# That is investigated in program 2 in the project flow


                          ### Bigger plots of response vs explanatories ###

# Response vs noncategorical explanatories
plot(DISTANCE,PRICEPTM) # Price decreases as distance increases. 
plot(WEIGHT,PRICEPTM)   # Price decreases as weight increases. 
plot(PCTLOAD,PRICEPTM) # The percentage follows the relationship between weight and prices.

# Response vs categorical explanatories
plot(ORIGIN,PRICEPTM) # JAX more expensive
plot(MARKET,PRICEPTM) # Price is higher for a large market.
plot(DEREG,PRICEPTM) # Price is higher for regulated shipments.
plot(CARRIER,PRICEPTM) # Prices vary with the truck carriers
plot(PRODUCT.f,PRICEPTM) # For product 100, 150, 200, the prices increase with each category.


### Histograms of response and each continuous covariate 
hist(PRICEPTM)
hist(DISTANCE) 
hist(WEIGHT)   
hist(PCTLOAD)  
# Due to the skew in response, we might consider a transformation later

##########################################################################################################
##########################################################################################################
                              ### Checking Collinearity ###


# Identify possible collinearity by looking at the pairwise correlation of each variable in the 
# model containing only non-categorical covariates
round(cor(trucking[, -c(1, 6, 7, 8, 9, 10)], use="pairwise.complete.obs"), 4)
# Observe that WEIGHT and PCTLOAD are strongly correlated.


                    ### Formal test of collinearity between WEIGHT and PCTLOAD ###

# Regress weight against the remaining covariates 
collinearcheck <- lm(WEIGHT~DISTANCE+PCTLOAD+ORIGIN+MARKET+DEREG+CARRIER+PRODUCT.f)
summary(collinearcheck) 
#R^2 = 0.9997. Formal evidence of a strong collinearity. Note: VIF > 3000 >> 10

# We should drop one of them to help with the stability of the LSE calculation.
# Which one should we drop? Consider a baseline model that includes all covariates, 
# with no transformation

# Basic LM, all explanatories
basicfit <- lm(PRICEPTM~DISTANCE+WEIGHT+PCTLOAD+ORIGIN+MARKET+DEREG+CARRIER+PRODUCT.f)
summary(basicfit)

#Then a lm with no PCTLOAD
nopctload <- lm(PRICEPTM~DISTANCE+WEIGHT+ORIGIN+MARKET+DEREG+CARRIER+PRODUCT.f)
summary(nopctload)

#Then a lm with no WEIGHT
noweight <- lm(PRICEPTM~DISTANCE+PCTLOAD+ORIGIN+MARKET+DEREG+CARRIER+PRODUCT.f)
summary(noweight)

# Out of nopctload and noweight, the model noweight performs marginally better in both R^2 and 
# adjusted R^2. This makes sense since PCTLOAD should capture signal from the WEIGHT variable already,
# and is put more in context because it also considers the carrying capacity of the truck too. 
# Thus we opt to remove WEIGHT.
# Remark: removal of either variable makes the adjusted R^2 lower than the baseline model. 
# This is acceptable since adding more covariates will always increase the R^2 anyway.

# AIC and BIC test
AIC(nopctload)
AIC(noweight)
BIC(nopctload)
BIC(noweight)
# noweight also has marginally better AIC and BIC than nopctload. This further supports the decision to 
# remove the weight


                  ### Significance test of Non-Binary Categorical Covariates ###

# Test the significance of CARRIER and PRODUCT.f on the "noweight" model:
reduced<-lm(PRICEPTM~DISTANCE+PCTLOAD+ORIGIN+MARKET+DEREG)
anova(noweight, reduced)
# Since p-value is smaller than 0.05, there is evidence reject the hypothesis that the 2 covariates 
# are not significant. We further test the covariates individually to see which covariate is insignificant.

# Test the significance of CARRIER
nocarrier<-lm(PRICEPTM~DISTANCE+PCTLOAD+ORIGIN+MARKET+DEREG+PRODUCT.f)
anova(noweight, nocarrier)
# The p-value is smaller than than 0.05, so here we reject the hypothesis that CARRIER is insignificant. 
# Hence we conclude that CARRIER needs to be in the model. 

# Test the significance of PRODUCT.f
noproduct<-lm(PRICEPTM~DISTANCE+PCTLOAD+ORIGIN+MARKET+DEREG+CARRIER)
anova(noweight, noproduct)
# The p-value is smaller than than 0.05, so here we reject the hypothesis that PRODUCT.f is insignificant. 
# Hence we conclude that PRODUCT.f needs to be in the model.


                      ### Residual vs. Non-Categorical Covariates Plot ###

# Residual vs. PCTLOAD
plot(PCTLOAD, residuals(noweight), main="Residual vs. PCTLOAD")

# Studentized Residual vs. PCTLOAD
plot(PCTLOAD, rstudent(noweight), main="Studentized Residual vs. PCTLOAD")
abline(2,0)
abline(-2,0)

# Residual vs. DISTANCE
plot(DISTANCE, residuals(noweight), main="Residual vs. DISTANCE")

# Studentized Residual vs. DISTANCE
plot(DISTANCE, rstudent(noweight), main="Studentized Residual vs. DISTANCE")
abline(2,0)
abline(-2,0)

##########################################################################################################
##########################################################################################################
                        ### Transformation Investigation ###

# As discussed, the response and some covariates are skewed. This suggests a transformation.

require("MASS")
require("car")

# First let's look at some diagnostics of the model we left off with in 2_collinearity.
# For clarity we call this pretransform
pretransform <- noweight

plot(pretransform, which=1:4)
# 1. residual vs. fitted shows a clear curvature
# 2. normal QQ performs poorly on the right tail
# 3. scale-location also shows a clear curvature
# 4. The 3 points identified by the Cook's distance are for super high PRICEPTM datapoints.
#    Note that on the axis, the Cook's D is still small (0.12 < 1), so no concern.
#    We can see that there are a bunch of points that have D > p/n, but correspond to expensive prices
#    This makes sense because truck companies charge higher prices to short distances/low % weight 
#    to discourage such inefficient use of trucks. So we choose to keep these points.

# Remark: The curvature in plot 1 and also the residuals vs leverage plot implies that there is
#         heteroscedasticity and non-normality. One way to rectify this is by applying a Box-Cox transform.


                                    ### Box-Cox Transform ###

# Consider a Box-Cox transform to rectify these issues and also the skew in the response variable

boxcox <- boxcox(pretransform, data = trucking, lambda = seq(-2, 2, 0.001)) 
lambda <- boxcox$x
loglik <- boxcox$y
optlambda <- lambda[which.max(loglik)] 
# The optimal lambda of -0.044 is close to zero. Zero is also contained in the Box-Cox 95% confidence 
# interval. Thus we pick lambda=0, which corresponds to a log transform applied onto the response.

transformed <- lm(log(PRICEPTM) ~ DISTANCE + PCTLOAD + ORIGIN + MARKET + DEREG + CARRIER + PRODUCT.f)
summary(transformed)
plot(transformed, 1:4)
# We see that the residuals-vs-fitted plot is flatter than before the transform, but still shows curvature. 
# The QQ plot is also better than before.
# The scale-location is still curving, let's see what else we can do.

# Let's look at the partial residual plots to see if there is a non-linear relationship that isn't being 
# captured in the model. We can see a curvature in the Lowess curves for DISTANCE and PCTLOAD 
# (especially DISTANCE). We might be observing economies of scale - an economics concept.
# We would expect the effect of PCTLOAD and DISTANCE on the Price per unit (ton-mile) to weaken
# as the PCTLOAD and DISTANCE increases. The trucking company benefits from cost savings because some 
# initial costs are fixed for a trucking trip (e.g. paperwork, personnel wages, etc).
# The per-unit cost of these fixed expenses decreases as production (PCTLOAD and DISTANCE) increases. 
# Traditionally these economics graphs are quadratic shaped, which is what we suspect here.

crPlots(m<-transformed, main="Partial Residual Plots", ylab=" Partial residual", smooth=T) 
# Note the curve in the green line!

plot(PCTLOAD, studres(transformed))  
plot(DISTANCE, studres(transformed)) 
# We definitely observe some curvatures in PCTLOAD and DISTANCE when we plot the studentized residual
# versus the covariate. This further supports including the quadratic terms.


                                    ### Quadratic Covariates ###

# We introduce the quadratic terms to account for the economies of scale
transformed2 <- lm(log(PRICEPTM)~poly(DISTANCE,2)+poly(PCTLOAD,2)+ORIGIN+MARKET+DEREG+CARRIER+PRODUCT.f)

summary(transformed2) 
# Adj. R^2 jumpst to 0.8179

plot(PCTLOAD, studres(transformed2))  # Curvature gone
plot(DISTANCE, studres(transformed2)) # Curvature gone (or at least much reduced)
plot(transformed2)


# Denote the transformed2 model by "logresp"
logresp<-transformed2

##########################################################################################################
##########################################################################################################
                                 ### Model Selection ###

# Consider the losresp model as the full model for model selection
fullfit <- logresp
# In the summary of logresp, it appears that ORIGIN perhaps can be dropped.
# We consider three model selections here:

# 1. manual selection (f-test)
# 2. forward selection (automatic)
# 3. stepwise regression (automatic)


                                    ### 1. Manual Selection ###

# We consider an f-test between the full logresp model and a reduced model without ORIGIN.
reducedfit <- update(fullfit, .~.-ORIGIN)
anova(fullfit,reducedfit)
# The p-value is 0.1385 > 0.05. There is no evidence against the hypothesis that the coefficients 
# of origin are 0. That is, no evidence against the hypothesis that it is insignificant. 
# Let's drop it from the model for the manual selection.
# Note that the p value isn't that big, so its the "weak evidence" grey area.

### How does the model perform now that we dropped the origin?
summary(reducedfit)
# Basically the same in terms of R^2 and standard error. But at least we have a cleaner model. 
# Maybe the AIC and BIC are better.

AIC(fullfit)
AIC(reducedfit)

BIC(fullfit)
BIC(reducedfit)

# AIC is marginally worse than the full fit, but the BIC is better for the reduced fit. 
# Since the BIC considers the size of the data in addition to the # of parameters, we elect to favour the
# BIC here. However the fact that AIC is beating means that the reduced and full must perform very closely.

model1 <- reducedfit
summary(model1)


                                ### 2. Forward Selection ###

options(digits=20)
null<-lm(log(PRICEPTM)~1)
newmodel<-addterm(null, scope=fullfit, test="F")

newmodel[2,6] # DISTANCE
newmodel[3,6] # PCTLOAD
newmodel<-lm(log(PRICEPTM)~poly(DISTANCE,2))
# lowest p value is DISTANCE. Add DISTANCE

addterm(newmodel, scope=fullfit, test="F")
addterm(newmodel, scope=fullfit, test="F")[2,6] # PCTLOAD
addterm(newmodel, scope=fullfit, test="F")[7,6] # PRODUCT.f
newmodel<-lm(log(PRICEPTM)~poly(DISTANCE,2)+poly(PCTLOAD,2))
# lowest p value is PCTLOAD. Add PCTLOAD

addterm(newmodel, scope=fullfit, test="F")
addterm(newmodel, scope=fullfit, test="F")[4,6] # DEREG
addterm(newmodel, scope=fullfit, test="F")[5,6] # CARRIER
addterm(newmodel, scope=fullfit, test="F")[6,6] # PRODUCT.f
newmodel<-lm(log(PRICEPTM)~poly(DISTANCE,2)+poly(PCTLOAD,2)+PRODUCT.f)
# lowest p value is PRODUCT.f. Add PRODUCT.f

addterm(newmodel, scope=fullfit, test="F")
addterm(newmodel, scope=fullfit, test="F")[4,6] # DEREG
addterm(newmodel, scope=fullfit, test="F")[5,6] # CARRIER
newmodel<-lm(log(PRICEPTM)~poly(DISTANCE,2)+poly(PCTLOAD,2)+PRODUCT.f+DEREG)
# lowest p value is DEREG. Add DEREG

addterm(newmodel, scope=fullfit, test="F")
newmodel<-lm(log(PRICEPTM)~poly(DISTANCE,2)+poly(PCTLOAD,2)+PRODUCT.f+DEREG+CARRIER)
# lowest p value is CARRIER. Add CARRIER

addterm(newmodel, scope=fullfit, test="F")
newmodel<-lm(log(PRICEPTM)~poly(DISTANCE,2)+poly(PCTLOAD,2)+PRODUCT.f+DEREG+CARRIER+MARKET)
# lowest p value is MARKET. Add MARKET

addterm(newmodel, scope=fullfit, test="F")

# ORIGIN's p-value is higher than 0.05, thus insiginificant. Stop here.

model2<-lm(log(PRICEPTM)~poly(DISTANCE,2)+poly(PCTLOAD,2)+MARKET+DEREG+CARRIER+PRODUCT.f)


                                  ### 3. Stepwise regression ###

# Consider a stepwise regression starting from a null model and working towards fullfit.
# Here we use the Akaike Information Criterion
nullmodel <- lm(log(PRICEPTM)~1)
step(nullmodel,scope=list(upper=fullfit),direction="both")
model3 <- lm(formula = log(PRICEPTM) ~ poly(DISTANCE,2) + poly(PCTLOAD,2) + PRODUCT.f + 
               DEREG + CARRIER + MARKET + ORIGIN)
# The stepwise regression chooses to keep the ORIGIN covariate.

# We pick models 1 (same as 2) and 3 to do the model diagnostics in the next program.

##########################################################################################################
##########################################################################################################
                          ### Diagnostic and Final Selection ###

require(caret)

### Here we select between two models:

# model A:lm(log(PRICEPTM) ~ poly(DISTANCE,2) + poly(PCTLOAD,2) + PRODUCT.f + DEREG + CARRIER + MARKET +
#                              ORIGIN)
# model B:lm(log(PRICEPTM) ~ poly(DISTANCE,2) + poly(PCTLOAD,2) + PRODUCT.f + DEREG + CARRIER + MARKET)

# Model A was suggested by the stepwise regression, and model B was selected by the forward selection
# and manual selection.

# Notice that the two models are identical except for the ORIGIN covariate. This
# is reassuring as opposed to having drastically different models that are predicting the same thing.
# We discuss which, out of these two similarly-performing models, should we finally select.

modelA <- model3
modelB <- model1 # note that model1 and model2 from 4_modelselection are the same.

# Quick ANOVA
anova(modelA,modelB)
# P value is 0.13, which under 0.05 means no evidence that we need origin... but at the same time the
# p value is not that high either. It is in a kind of grey area that maybe the ORIGIN is good.
# We need to evaluate this further.


                                    ### Model Evaluation ###

# Remark: The adjusted R^2 for modelA is marginally better than modelB. 
#         They are very close in this measure

summary(modelA)$adj.r.squared
summary(modelB)$adj.r.squared

### AIC and BIC
AIC(modelA)
AIC(modelB)

BIC(modelA)
BIC(modelB)

# AIC is marginally worse than the full fit, but the BIC is better for the reduced fit. 
# Since the BIC considers the size of the data in addition to the # of parameters, we elect to favour the
# BIC here. However the fact that AIC is beating means that the reduced and full must perform very closely.

### Assorted Residual Plots
plot(modelA, which=1:5)
plot(modelB, which=1:5)

# Residuals against each continuous covariate:
plot(DISTANCE,resid(modelA))
plot(PCTLOAD, resid(modelA))

plot(DISTANCE,resid(modelB))
plot(PCTLOAD, resid(modelB))
# Both covariates against both models look very similar. Model B ones have a little less curvature.


                                      ### 3-Fold Cross Validation ###

#Testing the prediction ability of the model covariates

# Q: Does including ORIGIN improve the predictive power of the model?
# Evaluation criteria: mean squared error
# Strategy: Do a 3-fold cross validation. Split data into 3 random folds. 
#           Fit on 2 out of 3 folds, predict on the remaining fold.
# Convert the predictions back to the regular scale and calculate a mean squared error for the holdout 1/3.

set.seed(333) 
# Splitting data into folds
folds <- createFolds(trucking$ID, k=3, list=TRUE, returnTrain = FALSE)

fold1 <- trucking[folds[[1]],]
fold2 <- trucking[folds[[2]],]
fold3 <- trucking[folds[[3]],]


# CROSS VALIDATION #1

# Fit models A and B on folds 1,2. Then, predict on 3
modelA_CV1 <- lm(data = rbind(fold1,fold2),formula = log(PRICEPTM) ~ poly(DISTANCE,2) + poly(PCTLOAD,2) + 
                   PRODUCT.f + DEREG + CARRIER + MARKET + ORIGIN)
modelB_CV1 <- lm(data = rbind(fold1,fold2),formula = log(PRICEPTM) ~ poly(DISTANCE,2) + poly(PCTLOAD,2) + 
                   PRODUCT.f + DEREG + CARRIER + MARKET)

# Remark: The predictions are converted back to regular scale through exponentiation
modelA_CV1_pred <- exp(predict(modelA_CV1,fold3))
modelB_CV1_pred <- exp(predict(modelB_CV1,fold3))

(modelA_CV1_MSE <- mean((fold3$PRICEPTM-modelA_CV1_pred)^2))
(modelB_CV1_MSE <- mean((fold3$PRICEPTM-modelB_CV1_pred)^2))
# RESULT: Model B has a lower MSE and is thus a better model in this CV


# CROSS VALIDATION #2

# Fit models A and B on folds 1,3. Then, predict on 2
modelA_CV2 <- lm(data = rbind(fold1,fold3),formula = log(PRICEPTM) ~ poly(DISTANCE,2) + poly(PCTLOAD,2) +
                   PRODUCT.f + DEREG + CARRIER + MARKET + ORIGIN)
modelB_CV2 <- lm(data = rbind(fold1,fold3),formula = log(PRICEPTM) ~ poly(DISTANCE,2) + poly(PCTLOAD,2) + 
                   PRODUCT.f + DEREG + CARRIER + MARKET)

# Remark: The predictions are converted back to regular scale through exponentiation
modelA_CV2_pred <- exp(predict(modelA_CV2,fold2))
modelB_CV2_pred <- exp(predict(modelB_CV2,fold2))

(modelA_CV2_MSE <- mean((fold2$PRICEPTM-modelA_CV2_pred)^2))
(modelB_CV2_MSE <- mean((fold2$PRICEPTM-modelB_CV2_pred)^2))
# RESULT: Model B has a lower MSE and is thus a better model in this CV


# CROSS VALIDATION #3

# Fit models A and B on folds 2,3. Then, predict on 1
modelA_CV3 <- lm(data = rbind(fold2,fold3),formula = log(PRICEPTM) ~ poly(DISTANCE,2) + poly(PCTLOAD,2) +
                   PRODUCT.f + DEREG + CARRIER + MARKET + ORIGIN)
modelB_CV3 <- lm(data = rbind(fold2,fold3),formula = log(PRICEPTM) ~ poly(DISTANCE,2) + poly(PCTLOAD,2) +
                   PRODUCT.f + DEREG + CARRIER + MARKET)

# Remark: The predictions are converted back to regular scale through exponentiation
modelA_CV3_pred <- exp(predict(modelA_CV3,fold1))
modelB_CV3_pred <- exp(predict(modelB_CV3,fold1))

(modelA_CV3_MSE <- mean((fold1$PRICEPTM-modelA_CV3_pred)^2))
(modelB_CV3_MSE <- mean((fold1$PRICEPTM-modelB_CV3_pred)^2))
# RESULT: Model A has a lower MSE and is thus a better model in this CV


                                      ### Final Selection ###

# Model B outperformed A in the cross validation in 2/3 folds.
# What this says is that the models A and B perform extremely similarly, as we saw before.
# However, most of the tests show that model B is slightly "better" in predictiveness since
# in the cross validation, model B was better 2/3 times. The P value in the beginning of 0.13 
# suggested we drop ORIGIN (model A) anyway.

# We (a little subjectively) choose to finally select model B.

finalmodel <- lm(formula = log(PRICEPTM) ~ poly(DISTANCE,2) + poly(PCTLOAD,2) + PRODUCT.f + DEREG +
                   CARRIER + MARKET)
summary(finalmodel)
plot(finalmodel, 1:4)  


                                        ### Outlier Check ###

# 3 influential cases: 119, 133, and 395. We identify these from the Cook's distance plot, 
# and the threshold p/n = 12/448 = 0.02678571
# As per discussion in section 3.3, only point 395 was removed

# Remove potential outliers
trucking2 <- trucking[-c(395),]
finalmodel2 <- lm(log(PRICEPTM) ~ poly(DISTANCE,2) + poly(PCTLOAD,2) + PRODUCT.f + DEREG + CARRIER + 
                    MARKET, data=trucking2)
summary(finalmodel2)
plot(finalmodel2, which=1:4)
confint(finalmodel2, level = 0.95)

##########################################################################################################
##########################################################################################################
                                    ### Final Discussion ###

# This final program uses the final model selected at the end of 5_diagnostic_finalselection
# finalmodel2 is the name of the object from the end of program 5

summary(finalmodel2)
coef(finalmodel2)

plot(trucking2$PRICEPTM,exp(fitted(finalmodel2))) # fitted vs actual plot
abline(a=1,b=1)


                                  ### Influence of Deregulation ### 

# Observe that the value of the coefficient for deregulation is -0.37596. Under the log scale, the 
# interpretation of this coefficient is that deregulated prices should cost 100(exp(-0.37596)-1)% = 31.33%
# less on average than regulated prices. Indeed, this makes sense because throughout history,
# for example in auto insurance, we have seen that after the regulatory body is removed, the average price 
# of the good decreased. This has been often attributed to competitive reasons between companies.

# We can furthermore give a 95% confidence interval for the effect of deregulation. That is equivalent to a
# 95% confidence interval for the value of the coefficient for deregulation. Such a confidence interval can
# then be backtransformed into a percent change for the effect on the price

CI_lower <- confint(finalmodel2, level = 0.95)[8,1]
CI_upper <- confint(finalmodel2, level = 0.95)[8,2]

print(c(CI_lower, CI_upper))

# We back-transform this CI to a confidence interval for the effect of deregulation
print(c(100*(exp(CI_lower)-1),100*(exp(CI_upper)-1)))

# A 95% confidence interval for the effect of deregulation on the price of trucking is a price REDUCTION
# of between 27.18747% and 35.24981%

# The covariates MARKET, CARRIER, and PRODUCT.f have similar interpretations.
# Market's coefficient means that small markets have an average price of 100(exp(-0.10674)-1)% = 10.12407%
# less than large markets (the baseline)
# Carriers B,C,D have average price of 100(exp(B)-1)% more (or less) than Carrier A (the baseline)
# Product classifications 150 and 200 have average price of 100(exp(B)-1)% more than product 
# code 100 (the baseline)

# The interpretations of DISTANCE and PCTLOAD are more complicated. The quadratic terms effectively 
# mean that the effects of DISTANCE and PCTLOAD on the PERCENT CHANGE in the price (because of the log) 
# vary as the values of DISTANCE and PCTLOAD change. The negative sign of the coefficients of the
# 2nd degree terms mean that as DISTANCE and PCTLOAD increase, the % change (deltas) in price diminish.

### Carrier B: Impact of deregulation on the representative case
(repcase <- trucking[230,])
repcase$DEREG <- c("NO      ") 
(B_no_dereg <- exp(predict.lm(finalmodel2, repcase)))
repcase$DEREG <- c("YES     ") 
(B_dereg <- exp(predict.lm(finalmodel2, repcase)))
(B_dereg/B_no_dereg-1)*100 # % change


                                        ### Interaction ### 

interaction.plot(DEREG, CARRIER, PRICEPTM)
interaction.plot(DEREG, PRODUCT.f, PRICEPTM)

##########################################################################################################