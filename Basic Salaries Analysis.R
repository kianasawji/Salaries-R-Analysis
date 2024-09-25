#attach all data points for initial exploratory analysis
salaries <- read.table("salaries.txt", head = T)
head(salaries, n=100)  #preview the data
attach(salaries, warn.conflicts = FALSE)

#fitting a simple linear regression model, regress salary on all other variables 
salaries.lm <- lm(Y ~X1+ X2+ X3+ X4+ X5+ X6+ X7+ X8+ X9+ X10 , data = salaries)

#plot the data
par(mfrow = c(1,2))
plot(salaries.lm)

#summary of the model
summary(salaries.lm)

#checking our model by performing a backwards stepwise selection
#data without age
drop1(salaries.lm, test="F")
model2 <- update(salaries.lm, ~ .-X7)
summary(model2)
plot(model2)

#data without age and board member
drop1(model2, test="F")
model3 <- update(model2, ~ .-X6)
summary(model3)
plot(model3)

#data without age, board member and company profits
drop1(model3, test="F")
model4 <- update(model3, ~ .-X8)
summary(model4)
plot(model4)

#data without age, board member, company profits and company sales
drop1(model4, test="F")
model5<-update(model4, ~ .-X10)
plot(model5)

#best final model according to the stepwise search
#fitting a simple linear regression by excluding some data points for the best model
salaries.bestlm <- lm(Y ~X1+ X2+ X3+ X4+ X5+ X9, data = salaries)
salaries.bestlm2 <- lm(Y ~I(X1^2)+ I(X2^2)+ X3+ X4+ X5+ X9, data = salaries)
summary(salaries.bestlm)
summary(salaries.bestlm2)
#assess linearity and normality with the plots for the new model
plot(salaries.bestlm)
plot(salaries.bestlm2)

#checking to see if the step function gives the same findings for the best model
step(salaries.lm)

#extracting the residuals from the data and comparing this to the observed values 
res <- resid(salaries.lm) # get list of residuals
plot(fitted(salaries.lm), res)

#checking if our data fits a normal distribution with a QQplot. 
plot(fit, standr, xlab = "Fitted values", ylab = "Standardised residuals")
qqnorm(standr, main = "", xlab = "Standardised residuals")
qqline(standr, col ="red")

#another way of model checking
standr <- rstandard(salaries.bestlm2)
fit <- fitted(salaries.bestlm)   

par(mfrow = c(1,1))

plot(fit, standr, xlab = "Fitted values", ylab = "Standardised residuals")
qqnorm(standr, main = "", xlab = "Standardised residuals")
qqline(standr, col ="red")
#this suggests that since the data points are following the line,
#they follow the assumed distribution

#anova table for the full model and best model
anova(salaries.lm)
anova(salaries.bestlm2)

#comparison for how well the model fits the data
#the lower the AIC the better
AIC(salaries.lm)
AIC(salaries.bestlm)
AIC(salaries.bestlm2)
AIC(model2)
AIC(model3)
AIC(model4)

#shows which predictor variables belong in the best regression model
#model for each possible combination with a certain number of predictor variables
#the stars indicate which should be paired together for 
#a certain number of predictor variables
library(leaps)
bestSubsets <- regsubsets(Y ~X1+ X2+ X3+ X4+ X5+ X6+ X7+ X8+ X9+ X10 , data = salaries)
summary(bestSubsets)
plot(bestSubsets)

#gives the adjusted r squared, mallows cp statistic and bic for each model
#the higher the better for adjr2, lower for bic and cp
#this has again confirmed that the best model is with 6 predictors
adjr2 <- summary(bestSubsets)$adjr2
summary(bestSubsets)$adjr2
summary(salaries.bestlm)$adjr2
       plot(adjr2, xlim = c(1,8), ylim = c(0.6, 1))

bic <- summary(bestSubsets)$bic
summary(bestSubsets)$bic
summary(salaries.bestlm)$bic
       plot(bic, xlim = c(1, 8), ylim = c(-230, -80))

cp <- summary(bestSubsets)$cp
summary(bestSubsets)$cp
summary(salaries.bestlm)$cp
       plot(cp, xlim = c(1, 8), ylim = c(4,355))

#last question we fit a lm to X3,X6, X9
#finding the confidence intervals for the linear regression model of salaries
confint(salaries.lm)

var1 <- median(salaries$X1)
var2 <- median(salaries$X2)
var4 <- median(salaries$X4)
var5 <- median(salaries$X5)

#setting all other variables to their median values in the same set as X3, X6, X9 
median_values <- data.frame(var1 = median(salaries$X1), 
                            var2 = median(salaries$X2),
                            var4 = median(salaries$X4),
                            var5 = median(salaries$X5), X3, X6, X9)

finalModel <- lm(Y ~var1+ var2+ X3+ var4+ var5+ X6+ X9, data = median_values)

#finding the 95% confidence intervals and stating the upper and lower bounds
prediction <- predict(finalModel, newdata = median_values, interval = "confidence", level = 0.95)
lower_bound <- predictions[, "lwr"]
upper_bound <- predictions[, "upr"]

confint(finalModel, level = 0.95)