# Lecture 1
### MASM22/FMSN30/FMSN40 spring 2018. Anna Lindgren ###
### lecture 1, 19/3-18 ###

# put ice-cream data from lecture 1 in a data frame:

icecream <- data.frame(weeks=c(25, 26,  27, 30, 31, 32, 35),
                       loss=c(28, 28.3, 29.7, 35.3, 36.4, 37.0, 40.2))

# and plot it:
with(icecream, plot(loss ~ weeks, 
                    xlab="time (weeks)", ylab="weight loss (g)",
                    xlim=c(23,38), ylim=c(25,45), main="Ice cream weight loss"))

### alternative, but longer version:
plot(icecream$loss ~ icecream$weeks, 
     xlab="time (weeks)", ylab="weight loss (g)",
     xlim=c(23,38), ylim=c(25,45), main="Ice cream weight loss")

# yet another version with x,y instead of y~x:
plot(icecream$weeks, icecream$loss, 
     xlab="time (weeks)", ylab="weight loss (g)",
     xlim=c(23,38), ylim=c(25,45), main="Ice cream weight loss")

# compute regression estimates "by hand"
mx <- mean(icecream$weeks)
my <- mean(icecream$loss)
beta1 <- sum((icecream$weeks-mx)*icecream$loss) / sum((icecream$weeks-mx)^2)
beta0 <- my - beta1*mx
beta0
beta1

# add the estimated straight line to the plot:
abline(a=beta0,b=beta1)

# estimated average weight loss at 34 weeks, by hand
y0 <- beta0 + beta1*34
y0
# add it to the plot;
points(34,y0,col="black",pch=16)

# residual standard error by hand:
yhat <- beta0+beta1*icecream$weeks
e <- icecream$loss-yhat
n <- nrow(icecream)
s2 <- sum(e^2)/(n-2)
s <- sqrt(s2)
s

# now using lm(): define an "object" named 'model'which can be reused later on
model <- lm(loss ~ weeks, data=icecream)  
model
summary(model)

abline(model,col="blue")

# predictions ising the predict function
x0 <- data.frame(weeks=34)
y0 <- predict(model,x0)
y0
points(x0$weeks,y0,col="blue",pch=16)

# predictions, residuals and redisual standard deviation
yhat <- model$fit
e <- model$residuals
s <- summary(model)$sigma

# Added
meanx <- mx


# 95% confidence intervals for parameters by hand:
# we need 's', the square root of the estimate of the error variance, see above
# 2.5%-quantile for a Student's distribution with n-2 degrees of freedom:
t_quant <- qt(1-0.05/2,n-2)
se_beta0 <- s*sqrt(1/n + meanx^2/sum((icecream$week-meanx)^2))
se_beta1 <- s/sqrt(sum((icecream$week-meanx)^2))
ci_beta0 <- beta0 + c(-1,1) * t_quant * se_beta0
ci_beta1 <- beta1 + c(-1,1) * t_quant * se_beta1

ci_beta0
ci_beta1

# or using confint()
# default uses 95% confidence level, otherwise use confint(model,level=0.9) etc.
confint(model)  

# Let's plot confidence and prediction intervals for E(Y0) and Y0 respectively
xx0 <- seq(23,38,0.1)  # just a grid of values for the predictor, from 23 to 38 
# ...with step 0.1
predx <- data.frame(weeks = xx0)
y1ci <- predict(model,predx,interval="confidence")
y1pi <- predict(model,predx,interval="prediction")

# add confidence interval lines
lines(xx0,y1ci[,"lwr"],lty=2,col="red",lwd=2)  # add confidence interval lines
lines(xx0,y1ci[,"upr"],lty=2,col="red",lwd=2)  # add confidence interval lines
lines(xx0,y1pi[,"lwr"],lty=3,col="blue",lwd=2)  # add prediction interval lines
lines(xx0,y1pi[,"upr"],lty=3,col="blue",lwd=2)  # add prediction interval lines
grid()

