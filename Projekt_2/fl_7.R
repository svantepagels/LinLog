## R-code for lecture 7:
# bugfix 25/4-18. AL

## create the example data set:
## 300 bananas (0), 70 apples (1), and 40 melons (2):
## 100 green (1) and 200 yellow (0) bananas, etc:
fruit <- data.frame(type=c(rep(0,each=300),rep(1,each=70),rep(2,each=40)),
                    green=c(rep(1,each=100),rep(0,each=200),
                            rep(1,each=40),rep(0,each=30),
                            rep(1,each=20),rep(0,each=20)))
fruit$type <- factor(fruit$type, levels=c(0,1,2),
                     labels=c("banana","apple","melon"))
fruit$green <- factor(fruit$green, levels=c(0,1), labels=c("yellow","green"))
summary(fruit)

# tabulate them:
fruit.table <- table(fruit$type, fruit$green)
fruit.table

### Estimates and intervals for p the "old fashioned" way
# See page 7 in Lecture 7:
pests <- data.frame(name=c("banana","apple","melon"),
                    n0 = c(fruit.table[1,1], fruit.table[2,1], fruit.table[3,1]),
                    n1 = c(fruit.table[1,2], fruit.table[2,2], fruit.table[3,2]))
pests$n <- pests$n0+pests$n1
pests$phat <- pests$n1/pests$n
pests$npq <- pests$n*pests$phat*(1-pests$phat)
pests$se <- sqrt(pests$phat*(1-pests$phat)/pests$n)
pests$lo <- pests$phat - 1.96*pests$se
pests$hi <- pests$phat + 1.96*pests$se
pests

########### NOW WITH LOGISTIC REGRESSION
## Fit a logistic regression:
model <- glm(green ~ type, data=fruit, family="binomial")
model
summary(model)

## beta-estimates (log odds(ratios)): 
model$coefficients

## exp(beta)-estimates (odds(ratios)): 
exp(model$coefficients)

# confidence interval for log-odds(ratios) beta
# ... using the sqrt(1/n + ...) version on page 13 in Lecture 7:
se.b0 <- sqrt(1/pests$n0[1]+1/pests$n1[1])
se.b1 <- sqrt(1/pests$n0[1]+1/pests$n1[1] + 1/pests$n0[2]+1/pests$n1[2])
se.b2 <- sqrt(1/pests$n0[1]+1/pests$n1[1] + 1/pests$n0[3]+1/pests$n1[3])
ci.beta <- rbind(model$coefficients[1]+1.96*se.b0*c(-1,1),
                 model$coefficients[2]+1.96*se.b1*c(-1,1),
                 model$coefficients[3]+1.96*se.b2*c(-1,1))
ci.beta
# and for exp(beta)
exp(ci.beta)

# confidence intervals for beta_j using the logistic model:
confint(model)
# confidence interval for odds(ratios): exp(beta)
exp(confint(model))

# PREDICT log-odds: beta0 + beta1*x = X*beta
x0 <- data.frame(type=c("banana","apple","melon"))

## log-odds, Xbeta:
predict(model,x0)

## log-odds with standard errors:
pred <- predict(model,x0, se.fit=T)
pred$fit
pred$se.fit

## confidence interval for log-odds
ci.logodds <- cbind(lo = pred$fit-1.96*pred$se.fit, hi=pred$fit+1.96*pred$se.fit)
ci.logodds

# predict probability of "success" (green) = exp(X*beta)/(1+exp(X*beta)):
predict(model, x0, type="response")

## confidence interval for probabilities:
exp(ci.logodds)/(1+exp(ci.logodds))
