# R-code for Lecture 8, vt18.

## plot on page 1:
data <- data.frame(x=seq(0,100,1))

data$logodds1 <- -5+0.1*data$x
data$odds1 <- exp(data$logodds1)
data$p1 <- data$odds1/(1+data$odds1)

data$logodds2 <- 5-0.1*data$x
data$odds2 <- exp(data$logodds2)
data$p2 <- data$odds2/(1+data$odds2)

data$logodds3 <- -10+0.5*data$x
data$odds3 <- exp(data$logodds3)
data$p3 <- data$odds3/(1+data$odds3)

data$logodds4 <- -6+0.1*data$x
data$odds4 <- exp(data$logodds4)
data$p4 <- data$odds4/(1+data$odds4)

data$logodds5 <- 1-0.04*data$x
data$odds5 <- exp(data$logodds5)
data$p5 <- data$odds5/(1+data$odds5)

with(data,{
  plot(p1~x, type="l", ylab="Pr(success)", lwd=2, main="Pr(success; x)")
  lines(x,p2, col="blue", lty=2,lwd=2)
  lines(x,p3, col="black", lty=3,lwd=2)
  lines(x,p4, col="darkorange", lty=4,lwd=2)
  lines(x,p5, col="magenta", lty=5,lwd=2)
})
legend(69,0.6,c("logit(p)= -5+0.1x", "logit(p)= 5-0.1x", 
                "logit(p)= -10+0.5x", "logit(p)= -6+0.1x",
                "logit(p)= 1-0.04x"),
       col=c("black", "blue","black","darkorange","magenta"),
       lty=c(1,2,3,4,5), lwd=2)

### generate random y-observations from p5:
set.seed(3) # for reproducibility
# NOTICE: unless you use set.seed, each time you execute the 
# rbinom command below you will obtain
# different draws hence a different dataset and different results 

data$y <- rbinom(n=nrow(data),size=1,prob=data$p5)

# Plot the data:
with(data, plot(y~x, main="Observed Y=0/1 against the covariate X"))

# calculate a kernel smoother and add it to the plot:
lines(ksmooth(data$x,data$y, bandwidth = 50))

# estimate the logistic model:
model <- glm(y~x, data=data, family="binomial")

# calculate the log odds and their CI:
phat <- predict(model, se.fit=T)
ci <- cbind(lo=phat$fit-1.96*phat$se.fit,
            hi=phat$fit+1.96*phat$se.fit)

# ... and transform them into probabilies
p <- exp(phat$fit)/(1+exp(phat$fit))
ci.p <- exp(ci)/(1+exp(ci))

# add them to the plot:
lines(p, col="blue")
lines(data$x,ci.p[,1], col="blue", lty=2)
lines(data$x,ci.p[,2], col="blue", lty=2)
legend(65,.8, c("kernel smoother", "p0-est", "95% C.i.(p0)"),
       col=c("black", "blue", "blue"), lty=c(1,1,2))

###### model summary
summary(model)

# calculating the null deviance "by hand":
my <- mean(data$y)
n <- nrow(data)
D0 <- -2*n*(my*log(my) + (1-my)*log(1-my))
D0

# LR-test for the full model vs the null model
anova(model)
Ddiff <- anova(model)$Deviance[2]
qchisq(1-0.05,1)
# p-value:
pchisq(Ddiff,1, lower.tail = F)

#################### Fruit data again:
fruit <- data.frame(type=c(rep(0,each=300),rep(1,each=70),rep(2,each=40)),
                    green=c(rep(1,each=100),rep(0,each=200),
                            rep(1,each=40),rep(0,each=30),
                            rep(1,each=20),rep(0,each=20)))
fruit$type <- factor(fruit$type, levels=c(0,1,2),
                     labels=c("banana","apple","melon"))
fruit$green <- factor(fruit$green, levels=c(0,1), labels=c("yellow","green"))
fruitmodel <- glm(green ~ type, data=fruit, family="binomial")
summary(fruitmodel)

anova(fruitmodel)
qchisq(1-0.05,2)
pchisq(anova(fruitmodel)$Deviance[2],2, lower.tail = F)