# Load data and packages
library(pscl)
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

# Seems reasonable when plotting the number of times ppm 
# exceeds the threshold

########################################################################################################

#############
## 3.2 (a) ##
#############

with(data=data, plot(cars~time))
# It seems likely that the PM_10 concentration is due to the number of cars

#############
## 3.2 (b) ##
#############

model.2 <- glm(highpm10 ~ cars, data=data, family="binomial")
or <- exp(model.2$coefficients)
ci.or <- exp(confint(model.2))

# Increase with 100 cars
(or[2])^100-1 # Hundred cars increases the odds by 4.9 %


# Prediction with standard errors
x0 <- data.frame(cars=c(min(data$cars): max(data$cars)))
pred.2 <- predict(model.2,x0, se.fit=T)

# Confidence interval for log-odds
ci.logodds.2 <- cbind(lo = pred.2$fit-1.96*pred.2$se.fit, hi=pred.2$fit+1.96*pred.2$se.fit)
ci.logodds.2 # Confidence interval for betas
exp(ci.logodds.2)
