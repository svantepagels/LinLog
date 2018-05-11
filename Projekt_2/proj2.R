# Load data and packages
library(pscl)
library(popbio)
library(plotly)
library(GGally)
library(ggplot2)
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

# Expected probabilities
exp(pred$fit)/(1+exp(pred$fit))

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
## 3.2 (b) ## Slide 3, lecture 8
#############

model.2 <- glm(highpm10 ~ cars, data=data, family="binomial")
or <- exp(model.2$coefficients)
ci.or <- exp(confint(model.2))

# Increase with 100 cars
(or[2])^(100)-1 # Hundred cars increases the odds by 4.87 % 

#############
## 3.2 (c) ##
#############

# Prediction with standard errors
x0 <- data.frame(cars=c(300, 3000))
pred.2 <- predict(model.2,x0, se.fit=T)

# Confidence interval for log-odds
ci.logodds.2 <- cbind(est = pred.2$fit, lo = pred.2$fit-1.96*pred.2$se.fit, hi=pred.2$fit+1.96*pred.2$se.fit)
ci.logodds.2 # Confidence interval for betas
exp(ci.logodds.2) # Confidence intervals for probabilities with different number of cars
exp(pred.2$fit) # Expected probabilities of PM_10 > 50 ppm


#############
## 3.2 (d) ##
#############

model.2.1 <- glm(highpm10 ~ log(cars), data=data, family="binomial")
# Odds ratio when log(cars) increases by one (write clearly in report)
or <- exp(model.2.1$coefficients)
ci.or <- exp(confint(model.2.1)) 

# Increase with 100 cars
(1100/1000)^model.2$coefficients[2] # Increase is 0.00454 %  


#############
## 3.2 (e) ##
#############

# Prediction with standard errors
x0 <- data.frame(cars=c(300, 3000))
pred.2.1 <- predict(model.2.1,x0, se.fit=T)

# Confidence interval for log-odds
ci.logodds.2.1 <- cbind(est = pred.2.1$fit, lo = pred.2.1$fit-1.96*pred.2.1$se.fit, hi=pred.2.1$fit+1.96*pred.2.1$se.fit)
ci.logodds.2.1 # Confidence interval for betas
exp(ci.logodds.2.1) # Confidence intervals for probabilities with different number of cars
exp(pred.2.1$fit) # Expected probabilities of PM_10 > 50 ppm
exp(ci.logodds.2.1)[,3]-exp(ci.logodds.2.1)[,2] # Width of conf. int. new model
exp(ci.logodds.2)[,3]-exp(ci.logodds.2)[,2] # Width of conf. int. old model

# We notice a slight decrease in the expected probabilities of exceeding the threshold
# when comparing to the previous model. The width of the confidence intervals are also slightly smaller
# than in the previous model. 

#############
## 3.2 (f) ## LUDVIG - Fler/andra mått? 
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
ksm <- ksmooth(data$cars, data$highpm10, bandwidth = 2000)
plot(ksm$x, ksm$y, type = 'l', xlab='Cars', ylab='Prob. high PM_10')
lines(prob ~ cars, newdat, col="green4", lwd=2)

# For "new" model
newdat <- data.frame(cars=seq(min(data$cars), max(data$cars),len=100))
newdat$prob = predict(model.2.1, newdata=newdat, type="response")
lines(prob ~ cars, newdat, col="red", lwd=2)

# The second model performs the best. 

#############
## 3.2 (h) ##
#############

newdat.2 <- newdat[order(newdat$prob),] 
newdat.2[min(which(newdat.2$prob > 0.1)-1),] # You can allow ~ 257 cars


#############
## 3.2 (i) ##
#############

# Cook's distance
Cooks_distance <- cooks.distance(model.2.1)
plot(Cooks_distance, xlab='Observation', ylab="Cook's distance")
which(Cooks_distance > 0.06) # Number 373 is the "worst"

# DFbetas, with 373 marked green
dfb <- dfbetas(model.2.1)
plot(dfb[,1], main="DFbeta intercept")
points(373,dfb[373,1],col="green", pch=19)

plot(dfb[,2], main="DFbeta beta_1")
points(373,dfb[373,2],col="green", pch=19)

# One observation that could be problematic: 373


########################################################################################################

#############
## 3.3 (a) ##
#############
# LUDVIG 
#############
## 3.3 (b) ##
#############

# Create full model and "empty" model

data$winddirection <- factor(data$winddirection, levels=c(1,2,3,4), labels=c("NE","SE","SW", "NW"))

fullmodel <- glm(highpm10 ~ log(cars)+temp2m+windspeed+
                   winddirection+time+log(windspeed),
                 data=data, family="binomial")
summary(fullmodel) #Since log(windspeed) is significant this will be used instead of windspeed

fullmodel.2 <- glm(highpm10 ~ log(cars)+temp2m+
                   winddirection+time+log(windspeed),
                 data=data, family="binomial")
emptymodel <- glm(highpm10 ~ 1, data=data, family="binomial")
summary(emptymodel)

backwards = step(fullmodel.2,k = log(nrow(data))) 
summary(backwards)

forwards = step(emptymodel,
                scope=list(lower=formula(emptymodel),upper=formula(fullmodel.2)), direction="forward",
                k=log(nrow(data)))
summary(forwards)

# Both the fowards and backwards stepping models propose the same choice of independent variables.

# Maybe winddirection can effect PM levels and interaction should be added? 
par(mfrow=c(1,1))
plot(data$winddirection~data$highpm10)

winddir_model <- glm(highpm10 ~ log(cars)+winddirection*log(windspeed),
                 data=data, family="binomial")
summary(winddir_model) # Nope, windirection interaction didn't add enough. 

# how about temperature?

temp_model <- glm(highpm10 ~ log(cars)+temp2m*log(windspeed),
                     data=data, family="binomial")
summary(temp_model) # Kind of good, lets check BIC

AIC(temp_model,k = log(nrow(data)))
AIC(forwards,k = log(nrow(data)))

# Better! Since data is relatively large the unsignificant predictor temp2m will be left. 

# Is it reasonable to add interaction term? 
data.test <- transform(data, temp_wind=temp2m*log(windspeed))
with(data.test, plot(highpm10~temp_wind)) 
ksm <- ksmooth(data.test$temp_wind, data.test$highpm10, bandwidth = 30)
temp_model <- glm(highpm10 ~ temp_wind,
                  data=data.test, family="binomial")
summary(temp_model)
newdat <- data.frame(temp_wind=seq(min(data.test$temp_wind), max(data.test$temp_wind),len=100))
plot(ksm$x, ksm$y, type = 'l', xlab='temp_wind', ylab='Prob. high PM_10')
newdat$prob = predict(temp_model, newdata=newdat, type="response")
lines(prob ~ temp_wind, newdat, col="green4", lwd=2)

# Yes, so it seems by fitting a model which's only independent variable is temp2m*log(windspeed)