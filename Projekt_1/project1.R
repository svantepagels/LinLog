# Project 1
raw <- read_excel("./data_project1.xlsx")
data <- as.data.frame(raw)

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
                    xlim=c(l_lim_x,u_lim_x), ylim=c(l_lim_y,u_lim_y), main="Title"))

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

plot(residuals~data$age) # Check eventual dependency against age
abline(h=0, col='red')

plot(residuals~model$fit) # Check eventaul dependency against fit
abline(h=0, col='red')

hist(residuals, breaks = 20)
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

plot(residuals2~model2$fit) # Check eventaul dependency against fit
abline(h=0, col='red')

hist(residuals2, breaks = 20)
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
