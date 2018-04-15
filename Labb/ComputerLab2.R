library(plyr)
# Load data
sleep <- read.delim("sleep.txt")

# Turn danger into categorical data
sleep$Danger <- factor(sleep$Danger, levels=c(1,2,3),
                       labels=c("low","medium","high"))

# Count frequency of danger
count(sleep$Danger)

# b) Calculate mean sleep for different danger categories
t <- aggregate(TotalSleep~Danger , data=sleep , FUN=mean)

# Boxplot
plot(TotalSleep~Danger, data=sleep)

# c) Multiple linear regression
model <- lm(TotalSleep~relevel(Danger,"low"), data=sleep)
summary(model)
# Means are coefficents in relation to beta_0
mean_low <- model$coefficients[1]
mean_medium <- mean_low + model$coefficients[2]
mean_high <- mean_low + model$coefficients[3]

unique(predict(model))

# Global F-test, t-statistics say that we might be able to merge low/medium
summary(model) #Look at F-statistic
anova(model) # Look at F-value and Pr(>F)
qf(1-0.05,2,54)

# Plot danger and body weight
with(data=sleep, plot(TotalSleep~BodyWt))
with(data=sleep, plot(TotalSleep~log(BodyWt))) # More reasonable since some animals weigh alot more

# Model with body weight and danger
model.w <- lm(TotalSleep~log(BodyWt), data=sleep)
summary(model.w) # Summary simple model with body weight
model.wd <- update(model.w , .~.+Danger)   
summary(model.wd) # Summary model with weight and danger

# Partial F-statistics
anova(model.w,model.wd) # F value = 10.356
qf(1-0.05,2,53)
# All betas are significant from t-test

# Confidence intervals
confint(model.wd)

# Predict homo sapiens total sleep
new_data <- data.frame(BodyWt = 62, Danger = 'low')
predict(model.wd, new_data)



# Difference between sleep is confidence interval of beta_high (?) 
confint(model.wd)[4,]

# Difference between Marmot Homo Sapiens
diff = log(62)-log(4)
confint(model.wd)[2,]*diff # Confidence interval
summary(model.wd)$coefficients[2,1]*diff # Expected difference in sleep

## Check answer with covariance matrix ##
#
XX <- summary(model.wd)$cov.unscaled
XX
