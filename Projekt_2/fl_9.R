## R-code for lecture 9:

# read the data
data<-read.table("f9_data.txt",header=T)
# variable x2 is a categorical variable:
data$x2 <- factor(data$x2, levels=c(0,1), labels=c("no","yes"))

# fit a logistic model using both covariates:
model<-glm(y ~ x1+x2,family="binomial",data=data)

summary(model)

# calculating the pseudo R2 "by hand":
an <- anova(model)
D0 <- an$`Resid. Dev`[1]
D <- an$`Resid. Dev`[3]
L0 <- exp(-D0/2)
L <- exp(-D/2)
n <- nrow(data)

# Cox-Snell:
R2CS <- 1 - (L0/L)^(2/n)
R2CS

# Nagelkerke:
R2max <- 1-(L0)^(2/n)
R2N <- R2CS/R2max
R2N

# using the pscl-package
# library("pscl")
# r2ML = Cox-Snell, r2CU = Nagelkerke
pR2(model)

######### Compating models:
model.0 <- glm(y~1, data=data, family="binomial")
model.1 <- glm(y~x1, data=data, family="binomial")
model.2 <- glm(y~x2, data=data, family="binomial")
model.3 <- glm(y~x1+x2, data=data, family="binomial")
model.4 <- glm(y~x1*x2, data=data, family="binomial")

summary(model.4) # has nonsignificant parameters
summary(model.3)

# Get the two different pseudo R2:
r2.0 <- pR2(model.0)[c(5,6)]
r2.1 <- pR2(model.1)[c(5,6)]
r2.2 <- pR2(model.2)[c(5,6)]
r2.3 <- pR2(model.3)[c(5,6)]
r2.4 <- pR2(model.4)[c(5,6)]
crit.models <- data.frame(name=c("1", "x1", "x2", "x1+x2", "x1*x2"),
                          R2CS = c(r2.0[1], r2.1[1], r2.2[1], r2.3[1], r2.4[1]),
                          R2N = c(r2.0[2], r2.1[2], r2.2[2], r2.3[2], r2.4[2]),
                          aic = AIC(model.0, model.1, model.2, model.3, 
                                    model.4)$AIC,
                          bic = AIC(model.0, model.1, model.2, model.3, model.4, 
                                    k=log(nrow(data)))$AIC)
crit.models

n.models <- nrow(crit.models)
x <- 1:n.models
with(crit.models, {
  plot(x,rep(0,n.models),type="n",ylim=c(0,1),xaxt="n",
       ylab="Pseudo R2",
       xlab="model",
       main="Pseudo R2")
  axis(1,at=x,labels=name)
  lines(x,R2CS,col="red",lty=2,lwd=2)
  lines(x,R2N,col="blue",lty=1,lwd=2)
  abline(h=R2max, col="black", lty=3, lwd=2)
})
legend(1,1,c("R2-Cox-Snell","R2-Nagelkerke", "R2CS-max"),
       col=c("red","blue","black"),
       lty=c(2,1,3),lwd=c(2,2,2))

summary(crit.models)
with(crit.models, {
  plot(x,rep(0,n.models),type="n",ylim=c(50,150),xaxt="n",
       ylab="AIC/BIC",
       xlab="model",
       main="Information criteria")
  axis(1,at=x,labels=name)
  lines(x,aic,col="red",lty=2,lwd=2)
  lines(x,bic,col="blue",lty=1,lwd=2)
})
legend(4,140,c("AIC","BIC"),
       col=c("red","blue"),
       lty=c(2,1),lwd=c(2,2))

##### Leverage and residuals for model 3: x1+x2
xbeta.3 <- predict(model.3)
inf.3 <- influence(model.3)

I <- data$x2=="no"
plot(exp(xbeta.3[I])/(1+exp(xbeta.3[I]))~data$x1[I], type="l", lwd=2,
     col="red",ylim=c(0,1), xlab="x1", ylab="estimated probability",
     main="Estimated probabilities")
lines(exp(xbeta.3[!I])/(1+exp(xbeta.3[!I]))~data$x1[!I], 
      col="blue", lty=2, lwd=2)
legend(2,.9,c("x2 = no", "x2 = yes"), lty=c(1,2), col=c("red","blue"), lwd=2)

hat.3 <- inf.3$hat
pear.3 <- inf.3$pear.res
std.3 <- inf.3$pear.res/sqrt(1-hat.3)
dev.3 <- inf.3$dev.res
stddev.3 <- dev.3/sqrt(1-hat.3)

summary(hat.3)
plot(hat.3[I]~data$x1[I], ylim=c(0,0.1), col="red", pch=1, xlab="x1", 
     ylab="leverage", main="Leverage")
points(hat.3[!I]~data$x1[!I],col="blue",pch=9)
legend(5,0.09, c("x2 = no", "x2 = yes"), col=c("red","blue"), pch=c(1,9))

plot(std.3~xbeta.3, xlab="linear predictor xbeta", 
     ylab="Standardized residuals", main="Standardized residuals")
abline(h=c(-2,0,2),lty=3)

plot(std.3^2~xbeta.3, xlab="linear predictor xbeta", 
     ylab="(Standardized residuals)^2", main="Squared Standardized residuals")
abline(h=4,lty=3)

plot(stddev.3~xbeta.3, xlab="linear predictor xbeta", ylim=c(-3,3),
     ylab="standardized deviance residuals",
     main="Standardized deviance residuals")
abline(h=c(-2,0,2),lty=3)

qqnorm(stddev.3, main="Normal Q-Q Plot: standardized deviance residuals")
qqline(stddev.3)


# Cook's distance and dfbetas:
cook.3 <- cooks.distance(model.3)
plot(cook.3, ylim=c(0,1), ylab="Cook's distance", main="Cook's distance")
abline(h=c(1,4/nrow(data)), lty=3)

summary(dfb)
dfb <- dfbetas(model.3)
plot(dfb[,1], ylim=c(-1, 1), ylab="dfbeta_0", main="dfbeta: Intercept")
abline(h=c(-1,0,-2/sqrt(nrow(data)),2/sqrt(nrow(data)),1), lty=3)

plot(dfb[,2], ylim=c(-1, 1), ylab="dfbeta_1", main="dfbeta: x1")
abline(h=c(-1,0,-2/sqrt(nrow(data)),2/sqrt(nrow(data)),1), lty=3)

plot(dfb[,3], ylim=c(-1, 1), ylab="dfbeta_2", main="dfbeta: x2")
abline(h=c(-1,0,-2/sqrt(nrow(data)),2/sqrt(nrow(data)),1), lty=3)

plot(dfb[I,2]~data$x1[I], col="red", ylim=c(-1, 1))
points(dfb[!I,2]~data$x1[!I], col="blue")
abline(h=c(-1,0,-2/sqrt(nrow(data)),2/sqrt(nrow(data)),1), lty=3)

plot(dfb[I,3]~data$x1[I], col="red", ylim=c(-1, 1))
points(dfb[!I,3]~data$x1[!I], col="blue")
abline(h=c(-1,0,-2/sqrt(nrow(data)),2/sqrt(nrow(data)),1), lty=3)

###### Goodness of fit

# predict success if phat>0.5 which is the same as xbeta>0:
yhat.3 <- xbeta.3>0
tabell.3 <- table(data$y,yhat.3)
tabell.3
ptable.3 <- prop.table(tabell.3, margin = 1)
ptable.3

yhat.1 <- predict(model.1)>0
tabell.1 <- table(data$y,yhat.1)
tabell.1
ptable.1 <- prop.table(tabell.1, margin = 1)
ptable.1

#########################################
