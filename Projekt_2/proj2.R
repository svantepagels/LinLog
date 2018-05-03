# Load data and packages
library(pscl)
library(popbio)
data <- read.delim("pm10.txt")

#############
## 3.1 (a) ##
#############

n <- nrow(data) 
p_hat <- sum(data$highpm10)/n

# Confidence interval when approximating as normally distributed since np(1 − p) > 10
error <- qnorm(0.975)*sqrt(p_hat*(1-p_hat)/n)
CI_left <- p_hat - error
CI_right <- p_hat + error

#############
## 3.1 (b) ##
#############

data$highpm10_not_cat <- data$highpm10
data$highpm10 <- factor(data$highpm10, levels=c(0,1),
                     labels=c("under_50", 'over_50'))
data$time <- factor(data$time, levels=c(1,2,3,4), labels=c("01-06","06-12","12-18", "18–24"))
with(data = data, plot(highpm10~time))
model <- glm(highpm10 ~ time, data=data, family="binomial")
summary(model)

# Confidence intervals
confint(model) # log odds ratios (beta)
exp(confint(model)) # odds ratios exp(beta)

#############
## 3.1 (c) ##
#############

# Prediction with standard errors
x0 <- data.frame(time=c('01-06','06-12','12-18','18–24'))
pred <- predict(model,x0, se.fit=T)
pred$fit # Betas
pred$se.fit # Standard error of betas

# Confidence interval for log-odds
ci.logodds <- cbind(lo = pred$fit-1.96*pred$se.fit, hi=pred$fit+1.96*pred$se.fit)
ci.logodds # Confidence interval for betas

# Confidence interval for probabilities:
exp(ci.logodds)/(1+exp(ci.logodds)) 

# Seems reasonable when plotting the number of times ppm exceeds the threshold

########################################################################################################

#############
## 3.2 (a) ##
#############

with(data=data, plot(cars~time))
# It seems likely that the PM_10 concentration is due to the number of cars

#############
## 3.2 (b) ## Håller du med? Slide 3, lecture 8. LUDVIG
#############

model.2 <- glm(highpm10 ~ cars, data=data, family="binomial")
or <- exp(model.2$coefficients)
ci.or <- exp(confint(model.2))

# Increase with 100 cars
(or[2])^100-1 # Hundred cars increases the odds by 4.9 %

#############
## 3.2 (c) ##
#############

# Prediction with standard errors
x0 <- data.frame(cars=c(300, 3000))
pred.2 <- predict(model.2,x0, se.fit=T)

# Confidence interval for log-odds
ci.logodds.2 <- cbind(lo = pred.2$fit-1.96*pred.2$se.fit, hi=pred.2$fit+1.96*pred.2$se.fit)
ci.logodds.2 # Confidence interval for betas
exp(ci.logodds.2) # Confidence intervals for probabilities with different number of cars
exp(pred.2$fit) # Expected probabilities of PM_10 > 50 ppm

#############
## 3.2 (d) ## Håller du med? Slide 3, lecture 8. LUDVIG
#############

model.2.1 <- glm(highpm10 ~ log(cars), data=data, family="binomial")
or <- exp(model.2.1$coefficients)
ci.or <- exp(confint(model.2.1))

# Increase with 100 cars
(or[2])^(log(100))-1 # Hundred cars increases the odds by 1433 % LUDVIG

#############
## 3.2 (e) ##
#############

# Prediction with standard errors
x0 <- data.frame(cars=c(300, 3000))
pred.2.1 <- predict(model.2.1,x0, se.fit=T)

# Confidence interval for log-odds
ci.logodds.2.1 <- cbind(lo = pred.2.1$fit-1.96*pred.2.1$se.fit, hi=pred.2.1$fit+1.96*pred.2.1$se.fit)
ci.logodds.2.1 # Confidence interval for betas
exp(ci.logodds.2.1) # Confidence intervals for probabilities with different number of cars
exp(pred.2.1$fit) # Expected probabilities of PM_10 > 50 ppm

# We notice a slight decrease in the expected probabilities of exceeding the threshold
# when comparing to the previous model. 

#############
## 3.2 (f) ##
#############

# Akaike
AIC(model.2)
AIC(model.2.1)
# r2ML = Cox-Snell, r2CU = Nagelkerke
pR2(model.2)
pR2(model.2.1)

# It seems that the second model performs better, both in terms of Akaike scoring and 
# pseudo R2 scoring. Since the number of explanatory variables are the same in both models
# AIC was chosen instead of BIC.

#############
## 3.2 (g) ##
#############

# For "old" model
newdat <- data.frame(cars=seq(min(data$cars), max(data$cars),len=100))
newdat$prob = predict(model.2, newdata=newdat, type="response")
ksm <- ksmooth(data$cars, data$highpm10_not_cat, bandwidth = 2000)
plot(ksm$x, ksm$y, type = 'l', xlab='Cars', ylab='Prob. high PM_10')
lines(prob ~ cars, newdat, col="green4", lwd=2)

# For "new" model
newdat <- data.frame(cars=seq(min(data$cars), max(data$cars),len=100))
newdat$prob = predict(model.2.1, newdata=newdat, type="response")
plot(ksm$x, ksm$y, type = 'l', xlab='Cars', ylab='Prob. high PM_10')
lines(prob ~ cars, newdat, col="green4", lwd=2)

# The second model performs the best. 

#############
## 3.2 (h) ##
#############

newdat.2 <- newdat[order(newdat$prob),] 
newdat.2[min(which(newdat.2$prob > 0.1)-1),] # You can allow ~ 257 cars


#############
## 3.2 (i) ##
#############
 