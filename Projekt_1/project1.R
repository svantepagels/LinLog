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

