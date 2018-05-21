# Project 1
library(readxl)
library(dplyr)
library(GGally)
library(ggplot2)
raw <- read_excel("./data_project1.xlsx")
data <- as.data.frame(raw)

###################################################################################################
#                                          3.1                                                    #
###################################################################################################

# We want to model how Plasma Retinol varies with Age. 
# Plot them against each other and determine, visualy, whether a linear relationship might be appropriate.
min_retplasma <- min(data$retplasma)
max_retplasma <- max(data$retplasma)
min_age <- min(data$age)
max_age <- max(data$age)

l_lim_y <- 0.9 * min_retplasma
u_lim_y <- 1.1 * max_retplasma
l_lim_x <- min_age * 0.9
u_lim_x <- max_age * 1.1

with(data, plot(retplasma ~ age, 
                    xlab="Age", ylab="Plasma Retinol",
                    xlim=c(l_lim_x,u_lim_x), ylim=c(l_lim_y,u_lim_y)))

# Fit the model retplasma = ??0 + ??1 ?? age + ?? and report the ??-estimates together with their 95% confidence intervals.
# Are the parameters significant?
model <- lm(retplasma ~ age, data = data)  
summary(model)
abline(model,col="blue")
confint(model)

# What happens, on average, to the Plasma Retinal level if we increase the age by 1 year? 
# Calculate a 95 % confidence interval for the change. 
# Does the size of the change depend on the age?
# E[Beta1] is 3.0337 and confidence interval is:
confint(model)[2,]
# Doesn't depend on age since linear regression model. 


#Investigate the residuals. Do they fulfil the model asumptions? If not, what seems to be the problem?
residuals <- resid(model)
plot(residuals) # Check randomly scattered around zero

plot(residuals~data$age, xlab='Age', ylab='Residuals') # Check eventual dependency against age
abline(h=0, col='red')

plot(residuals~model$fit) # Check eventaul dependency against fit
abline(h=0, col='red')

hist(residuals, breaks = 20, xlab='Residuals')
abline(v=mean(residuals), col='red', lty=3, lwd=3) # Slightly skewed residuals

# Check normality 
qqnorm(residuals)
qqline(residuals) # Residuals right-skewed, high values higher due to long heavier tail, 
# low values not as "extreme" as expected

#Ignoring any problems with the residuals, calculate the 95% confidence interval 
#for the expected Plasma Retinol for ages 18,...,85 and add it to the plot of the data, 
#together with the estimated relationship. 
#Do the problems with the residuals seem to have an implact?
new_data <- data.frame(age = 18:85)
prediction <- predict(model, new_data, interval='confidence')
with(data, plot(retplasma ~ age, 
                xlab="Age", ylab="Plasma Retinol",
                xlim=c(l_lim_x,u_lim_x), ylim=c(l_lim_y,u_lim_y), main="Age and Retinol"))
lines(prediction[,1]~new_data$age,col="green", lwd=1.5)
lines(prediction[,2]~new_data$age,col="red", lwd=1.5, lty=3)
lines(prediction[,3]~new_data$age,col="red", lwd=1.5, lty=3)

#Calculate the 95 % prediction interval for the expected Plasma Retinol for ages 18,. . . ,85 and add it to the plot. 
#Do you find any problems here? How does this relate to the problem with the residuals?
prediction2 <- predict(model, new_data, interval='prediction')
with(data, plot(retplasma ~ age, 
                xlab="Age", ylab="Plasma Retinol",
                xlim=c(l_lim_x,u_lim_x), ylim=c(l_lim_y,u_lim_y), main="Age and Retinol"))
lines(prediction2[,1]~new_data$age,col="green", lwd=1.5)
lines(prediction2[,2]~new_data$age,col="red", lwd=1.5, lty=3)
lines(prediction2[,3]~new_data$age,col="red", lwd=1.5, lty=3)
# Almost all outlier data-points are above our confidence interval.

#Report a 95 % prediction interval for the observed Plasma Retinol of a 30 year old person, as well as for a 70 year old person. 
#Are there any substantial differences in the widths of the two intervals? Why or why not?

thirty <- predict(model, data.frame(age=30), interval='prediction')
abs(thirty[,2]-thirty[,3]) # Width of confidence interval
seventy <- predict(model, data.frame(age=70), interval='prediction')
abs(seventy[,2]-seventy[,3]) # Width of confidence interval

###################################################################################################
#                                          3.2                                                    #
###################################################################################################


# Investigate whether taking the logarithm of the Plasma Retinol might improve the model fit and reduce
# the problems with the residuals by fitting the new model log(retplasma) = ??0 + ??1 ?? age + ??. Report
# the estimates and their confidence intervals. Are they significant?

# Taking adding a column in data for logarithm Plasma Retinol
data.logRet <- data
data.logRet$logRet <- log(data$retplasma)

# Plot
with(data.logRet, plot(logRet ~ age, 
                xlab="Age", ylab="log Plasma Retinol",
                main="log Retinol and Age"))

# Linear fit
model2 <- lm(logRet ~ age, data = data.logRet)  
summary(model2)
confint(model2)
abline(model2, col='blue')

# Investigate the new residuals. Do they fulfill the model assumptions? Did the transformation solve the
# problems?

residuals2 <- resid(model2)

plot(residuals2~model2$fit, ylab='Residuals', xlab='Fitted values') # Check eventaul dependency against fit
abline(h=0, col='red')

hist(residuals2, breaks = 20, xlab='Residuals', main='Histogram of residuals')
abline(v=mean(residuals2), col='red', lty=3, lwd=3) # Slightly skewed residuals

# Check normality 
qqnorm(residuals2)
qqline(residuals2) 

# The residuals seem to have improved but do still exhibit fatter tails than a normal distribution.
# Modelling residuals as student-t distributed could be a good approach to get rid of this. 


# Write down how Plasma retinol depends on age and plot the data again, adding this new, non-linear,
# fit. What happens, on average, to the Plasma Retinal level if we increase the age by 1 year? Calculate a
# 95 % confidence interval for this change. Does the size of the change (in ng/ml) depend on the age?

# Plot exp( fitted values ) against age
with(data, plot(retplasma ~ age, 
                xlab="Age", ylab="Plasma Retinol",
                xlim=c(l_lim_x,u_lim_x), ylim=c(l_lim_y,u_lim_y), main="Title"))
lines(exp(model2$fit)~age, data=data.logRet, col='red')

plot(exp(model2$fit)~age, data=data.logRet, col='red')

# Since the values of beta1*age_i range between such low values, the dependency between age and ret almost linear.
# Although the dependency is actually exponential. 

# Calculate the 95 % confidence interval for the expected log Plasma Retinol for ages 18,. . . ,85 and
# transform it into the original scale. Add it to the plot of the data, Any major differences compared to
# the previous model? Why or why not?

new_data <- data.frame(age = 18:85)
prediction3 <- exp(predict(model2, new_data, interval='confidence'))
with(data, plot(retplasma ~ age, 
                xlab="Age", ylab="Plasma Retinol",
                xlim=c(l_lim_x,u_lim_x), ylim=c(l_lim_y,u_lim_y), main="Age and log Retinol"))
# Confidence intervals without transformation
lines(prediction[,1]~new_data$age,col="green", lwd=1.5)
lines(prediction[,2]~new_data$age,col="red", lwd=1.5, lty=3)
lines(prediction[,3]~new_data$age,col="red", lwd=1.5, lty=3)
# Confidence intervals with transformation
lines(prediction3[,1]~new_data$age,col="pink", lwd=1.5)
lines(prediction3[,2]~new_data$age,col="blue", lwd=1.5, lty=3)
lines(prediction3[,3]~new_data$age,col="blue", lwd=1.5, lty=3)

# Calculate the 95 % prediction interval for the expected log Plasma Retinol for ages 18,. . . ,85, transform
# it into the original scale and add it to the plot. Do you find any problems here now? Any major
# differences compared to the previous model?

prediction4 <- exp(predict(model2, new_data, interval='prediction'))
with(data, plot(retplasma ~ age, 
                xlab="Age", ylab="Plasma Retinol",
                xlim=c(l_lim_x,u_lim_x), ylim=c(l_lim_y,u_lim_y), main="Age and Retinol"))
# Confidence intervals without transformation
lines(prediction2[,1]~new_data$age,col="green", lwd=1.5)
lines(prediction2[,2]~new_data$age,col="red", lwd=1.5, lty=3)
lines(prediction2[,3]~new_data$age,col="red", lwd=1.5, lty=3)
# Confidence intervals with transformation
lines(prediction4[,1]~new_data$age,col="pink", lwd=1.5)
lines(prediction4[,2]~new_data$age,col="blue", lwd=1.5, lty=3)
lines(prediction4[,3]~new_data$age,col="blue", lwd=1.5, lty=3)

# Report a 95 % prediction interval for the observed Plasma Retinol of a 30 year old person, as well a
# for a 70 year old person. Are there any substantial differences in the widths of the two intervals? Why
# or why not?

thirty <- exp(predict(model2, data.frame(age=30), interval='prediction'))
abs(thirty[,2]-thirty[,3]) # Width of confidence interval
seventy <- exp(predict(model2, data.frame(age=70), interval='prediction'))
abs(seventy[,2]-seventy[,3]) # Width of confidence interval

# R-squared scores - for fun
summary(model2)[8]
summary(model2)[9]
summary(model)[8]
summary(model)[9]

###################################################################################################
#                                          3.3                                                    #
###################################################################################################

# a) 
model3 <- lm(betaplasma ~ age, data = data)
residuals3 <- resid(model3)
hist(residuals3, breaks=20, xlab='Residuals', main='Histogram of residuals')

newdata <- subset(data, betaplasma>0)
model3.2 <- lm(log(betaplasma) ~ age, data = newdata)
residuals3.2 <- resid(model3.2)
hist(residuals3.2, breaks=20, xlab='Residuals', main='Histogram of residuals')

# b) 
newdata$smokstat <- factor(newdata$smokstat, levels=c(1,2,3), 
                           labels = c('never', 'former', 'current'))
model3.3 <- lm(betaplasma ~ smokstat, data = newdata)
summary(model3.3)

model3.4 <- lm(betaplasma ~ relevel(smokstat, ref = 3), data = newdata)
summary(model3.4)

model3.4.1 <- lm(betaplasma ~ relevel(smokstat, ref = 2), data = newdata)
summary(model3.4.1)

# Standard error increased. Fewer smokers lead to more uncertainty. 

newdata$sex <- factor(newdata$sex, levels=c(1,2), 
                           labels = c('male', 'female'))
model3.5 <- lm(betaplasma ~ sex, data = newdata)
summary(model3.5)

model3.6 <- lm(betaplasma ~ relevel(sex, ref = 2), data = newdata)
summary(model3.6)

# 'female' has lower st error, since more prevailent. 

newdata$vituse <- factor(newdata$vituse, levels=c(1,2,3), 
                           labels = c('often', 'not often', 'no'))
model3.7 <- lm(betaplasma ~ vituse, data = newdata)
summary(model3.7)

model3.8 <- lm(betaplasma ~ relevel(vituse, ref = 2), data = newdata)
summary(model3.8)

model3.9 <- lm(betaplasma ~ relevel(vituse, ref = 3), data = newdata)
summary(model3.9)

# 'often' or 'no' should be chosen

# c)
with(data=newdata, plot(log(betaplasma) ~ alcohol, ylab='log beta-carotene', xlab='Alcohol consumption') )

# Problem: outlier. Bad idea since many users do not consume alcohol. 

newdata$id <- c(1:nrow(newdata))
newdata$alcohol <- as.double(newdata$alcohol) 
outlier <- newdata[which(newdata$alcohol>200), 15] # Outlier is 62

par(mfrow=c(2,2)) # Combine the four plots bellow

newdata$calories <- as.double(newdata$calories)
with(data=newdata, plot(log(betaplasma) ~ calories, ylab='log beta-carotene') )
points(newdata$calories[outlier],log(newdata$betaplasma[outlier]),col="green", pch=19)

newdata$quetelet <- as.double(newdata$quetelet) 
with(data=newdata, plot(log(betaplasma) ~ quetelet, ylab='log beta-carotene') )
points(newdata$quetelet[outlier],log(newdata$betaplasma[outlier]),col="green", pch=19)

newdata$fat <- as.double(newdata$fat) 
with(data=newdata, plot(log(betaplasma) ~ fat, ylab='log beta-carotene') )
points(newdata$fat[outlier],log(newdata$betaplasma[outlier]),col="green", pch=19)

newdata$fiber <- as.double(newdata$fiber) 
with(data=newdata, plot(log(betaplasma) ~ fiber, ylab='log beta-carotene') )
points(newdata$fiber[outlier],log(newdata$betaplasma[outlier]),col="green", pch=19)


# Continuos explanatory variables: age, quetelet, calories, fat, fiber, alcohol, cholesterol, betadiet
newdata$age <- as.double(newdata$age)
newdata$cholesterol <- as.double(newdata$cholesterol)
newdata$betadiet <- as.double(newdata$betadiet)

lin_rel <- newdata %>% select(-sex, -vituse, -smokstat, -retplasma, -retdiet, -retplasma, -id)
ggpairs(lin_rel, mapping=ggplot2::aes(colour='blue'), axisLabels='none')



###################################################################################################
#                                          3.4                                                    #
###################################################################################################

# Fit a model using the background variables (age, sex, smokstat and quetelet), report the parameter
# estimates and their corresponding 95 % confidence intervals. Also determine whether all the
# variables have a significant contribution to the model.
# Use the fitted model to construct an interval that would be expected to contain 95 % of the Plasma
# beta-carotene values of persons that are 50 years old, male, have never smoked and have a BMI of 30.

model4 <- lm(log(betaplasma) ~ age+sex+smokstat+quetelet, data = newdata) 
summary(model4) # Only smokstat-former not significant
confint(model4)

new_data <- data.frame(age = 50, sex='male', smokstat='never', quetelet=30)
exp(predict(model4, new_data, interval='confidence'))

# Fit a model using the dietary factors (vituse, calories, fat, fiber, alcohol, cholesterol and betadiet) instead.
# Are all the variables significant? If not, use a stepwise procedure to reduce the model. 
# Report the parameter estimates and the corresponding confidence intervals of the reduced
# model. Use the reduced model to construct an interval that would be expected to contain 95 % of the Plasma
# beta-carotene values of persons that never eat vitamins, consume 1200 calories, 50 grams of fat,
# 20 grams of fiber, no alcohol, 300 mg cholesterol and 1500 mcg dietary beta-carotene per day.

model4.1 <- lm(log(betaplasma) ~vituse+calories+fat+fiber+alcohol+cholesterol+betadiet, data = newdata) 
summary(model4.1)
confint(model4.1)

model4.2 <- lm(log(betaplasma) ~1, data = newdata) 

model4.reduced <- step(model4.1,scope=list(lower=model4.2, upper=model4.1),k=log(nrow(newdata)))
summary(model4.reduced)
confint(model4.reduced)

# Best model: log(betaplasma) ~ vituse + calories + fiber

new_data <- data.frame(vituse='no', calories=1200, fiber=20)
exp(predict(model4.reduced, new_data, interval='confidence'))

# We now have two competing models, the background variables model in (a) and the (reduced)
# dietary factors model in (b). Compare the two models regarding, e.g., their ability to explain the
# variability in Plasma beta-carotene. Which model seems best?
# Try to find a better model using both some of the background variables and some of the dietary factors.
# Compare its ability to explain the variability to the models from (a) and (b).

summary(model4)[9]
summary(model4.reduced)[9]
summary(model4.reduced.2)[9]

AIC(model4, model4.reduced.2, model4.reduced, k=log(nrow(newdata)))

model4.3 <- lm(log(betaplasma) ~vituse+calories+fat+fiber+alcohol+cholesterol+betadiet+age+sex+smokstat+quetelet, 
               data = newdata) 
summary(model4.3)

model4.reduced.2 <- step(model4.3,scope=list(lower=model4.1, upper=model4.3),k=log(nrow(newdata)))
summary(model4.reduced.2)
summary(model4.reduced)
AIC(model4.reduced.2, model4.reduced, k=log(nrow(newdata)))
AIC(model4.reduced.2, model4.reduced)

# Now we turn our attention to the, possibly, problematic extreme alcohol consumer by investigating
# the leverage, studentized residuals, Cook???s distance and DFbetas of the (reduced) dietary factors model
# in (b). Has the person had any problematic influence on the model estimates? Are there any other
# persons that have had a problematic influence?

# Plotting leverage against i and all the X's
v <- hatvalues(model4.reduced) # leverage
plot(v)
points(outlier,v[outlier],col="green", pch=19) # Big impact on leverage
plot(v~newdata$vituse)
points(newdata$vituse[outlier],v[outlier],col="green", pch=19) 
plot(v~newdata$calories)
points(newdata$calories[outlier],v[outlier],col="green", pch=19) # Likely due to imense calory intake
plot(v~newdata$fiber)
points(newdata$fiber[outlier],v[outlier],col="green", pch=19) 

which(v > 0.07) # 51, 263 potential problematic

# Studentised residuals, plotted against i and independent variables
r_stud <- rstudent(model4.reduced) # studentised residuals
plot(r_stud, xlab='i', ylab='Studentised res')
abline(h=0)
abline(h=c(-2,2),col="red")
points(outlier,r_stud[outlier],col="green", pch=19) 

which(r_stud < -3) # 233 potential problematic

plot(r_stud~newdata$vituse)
abline(h=0)
abline(h=c(-2,2),col="red")
points(newdata$vituse[outlier],r_stud[outlier],col="green", pch=19) 

plot(r_stud~newdata$calories)
abline(h=0)
abline(h=c(-2,2),col="red")
points(newdata$calories[outlier],r_stud[outlier],col="green", pch=19) 

plot(r_stud~newdata$fiber)
abline(h=0)
abline(h=c(-2,2),col="red")
points(newdata$fiber[outlier],r_stud[outlier],col="green", pch=19)

# Cook's distance
Cooks_distance <- cooks.distance(model4.reduced)
plot(Cooks_distance, xlab=FALSE, ylab=FALSE, main = "Cook's distance")
points(outlier,Cooks_distance[outlier],col="green", pch=19)
abline(h=4/nrow(newdata), col='red') # Limit for large datasets

which(D > 0.03) # Potential problematic: 97, 233

# DFbetas

dfb <- dfbetas(model4.reduced)
plot(dfb[,1], ylab='Beta_0', main='DFbeta 0') 
points(outlier,dfb[outlier,1],col="green", pch=19) # Our alcoholic friend has big impact
which(abs(dfb[,1]) > 0.2)  
plot(dfb[,2]) 
points(outlier,dfb[outlier,2],col="green", pch=19)
which(abs(dfb[,2]) > 0.2) 
plot(dfb[,3]) 
points(outlier,dfb[outlier,3],col="green", pch=19)
which(abs(dfb[,3]) > 0.2)  
plot(dfb[,4], ylab='Beta_3', main='DFbeta')
points(outlier,dfb[outlier,4],col="green", pch=19) # Our alcoholic friend has big impact
which(abs(dfb[,4]) > 0.2) 
plot(dfb[,5], ylab='Beta_4', main='DFbeta')
points(outlier,dfb[outlier,5],col="green", pch=19) # Our alcoholic friend has big impact
which(abs(dfb[,5]) > 0.3) 

# Potential problematic other observations: 
# * 263 - high leverage, affecting beta4
# * 233 - high Cook's distance and unlikely studentised quantile, affecting beta1
# * 97 - high Cook's distance, affecting beta4


### Notes ###
# Interperting QQ-plot https://www.youtube.com/watch?v=-KXy4i8awOg&t=1s
