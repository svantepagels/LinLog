# Computer exersise 1

# Load data
emission <- data.frame(vehicles = c(28,36,15,-19,-24,8,25,40,63,12,-6,21),
                       pollution= c(22,26,15,-18,-21,7,21,31,52,8,-7,20))

summary(emission)

#Exercise 1: Air pollution

#(a) From the description above, which variable is the ”response variable”?
# Answer/comment: Level of pollution

#(b) Obtain a plot of the data with the response variable on the y-axis. Convince yourself that a linear model seems a sensible choice.
with(emission, plot(pollution ~ vehicles, 
                    xlab="Change in flow of vehicles (%)", ylab="Change in level of air pollution (%)",
                    xlim=c(-30,70), ylim=c(-30,70), main="Air pollution"))
# Answer/comment: Seems linear

#(c) Define a simple linear regression model of the response variable on the chosen explanatory variable (covariate). Use R to fit the linear model, then add the estimated model to the plot with data. Does it seem reasonable?
model <- lm(pollution ~ vehicles, data=emission)  
summary(model)

abline(model,col="blue")
# Answer/comment: Yes, seems reasonable

#(d) Check that the residuals follow the assuptions.
residuals <- resid(model)
# Check independece 
plot(residuals)
abline(h = 0, col = "blue")
# Dist?
hist(residuals) # Add normal curve
# Other way
qqnorm(residuals)
qqline(residuals)
# Answer/comment: Seems independent, normal dist cannot be judged with few data points.
#(e) Obtain the estimated regression coefficients and the unbiased estimate of the error variance σ2.
summary(model)
coefficients <- model$coefficients
s <- summary(model)$sigma

# Control by hand
mx <- mean(emission$vehicles)
my <- mean(emission$pollution)
beta1 <- sum((emission$vehicles-mx)*emission$pollution) / sum((emission$vehicles-mx)^2)
beta0 <- my - beta1*mx
beta0
beta1

yhat <- beta0+beta1*emission$vehicles
e <- emission$pollution-yhat
n <- nrow(emission)
s2 <- sum(e^2)/(n-2)
s <- sqrt(s2)
s

#(f) Test the significance of the hypothesis that the expected change in air pollution is zero (H0) when there is no change in the vehicle flow, as opposed to the expected change in air pollution being different from zero (H1).
# Interested in intercept p-value: 0.271
summary(model)
#(g) Derive the standard error of the coefficient of the ”vehicles” variable and use it to construct a 95 % confidence interval for the ”vehicles” parameter. Compare your result with the interval provided by R.
t_quant <- qt(1-0.05/2,n-2)
se_beta0 <- s*sqrt(1/n + mx^2/sum((emission$vehicles-mx)^2))
se_beta1 <- s/sqrt(sum((emission$vehicles-mx)^2))
ci_beta0 <- beta0 + c(-1,1) * t_quant * se_beta0
ci_beta1 <- beta1 + c(-1,1) * t_quant * se_beta1

ci_beta0
ci_beta1
#Comparison
confint(model)
#(h) We might expect that a specific change in the vehicle flow would result in an equally large change in the level of air pollution, i.e., β1 = 1. Test whether data support this conjecture.
pt( (beta1-1)/(se_beta1), n-2) 
# Should be multiplied by 2, now just one-sided

#(i) Give an estimate and a 95 % confidence interval for the expected change in air pollution when the flow of vehicles changes by 30 percent.

new_data_point <- data.frame(vehicles = 30)
predict(model, new_data_point, interval="confidence", level = 0.95)

#(j) Construct a 90 % prediction interval for the change in air pollution when the flow of vehicles changes by 30 percent.

new_data_point <- data.frame(vehicles = 30)
predict(model, new_data_point, interval="prediction", level = 0.90)

