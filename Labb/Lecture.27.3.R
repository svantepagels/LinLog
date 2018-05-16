### MASM22/FMSN30/FMSN40 spring 2018. Anna Lindgren ###
### lecture 4, 27/3-18 ### bugfix in interaction plot 27/3-18 ##
### Categorical variables. Global and partial F-test ###

cabbage <- data.frame(soil = c(rep(1,10),rep(2,10),rep(3,10)),
                      fertilize = c(10, 15, 20, 25, 30, 40, 45, 50, 55, 60,
                                    10, 15, 20, 25, 30, 40, 45, 50, 55, 60,
                                    10, 15, 20, 25, 30, 40, 45, 50, 55, 60),
                      headwt = c(1.4, 1.9, 3.4, 2.4, 4.2, 1.9, 1.7, 4.7,
                                 1.9, 2.0,
                                 3.1, 2.9, 3.6, 3.9, 3.6, 4.0, 3.3, 4.3,
                                 5.3, 4.6,
                                 1.9, 2.0, 3.1, 3.5, 3.3, 2.4, 2.5, 3.5,
                                 1.9, 3.3))
## tell R that soil is a categorical variable, not numerical:
cabbage$soil <- factor(cabbage$soil,
                       levels=c(1,2,3),
                       labels=c("sand","clay","loam"))

### model with categorical soil
with(cabbage,plot(headwt~soil,ylim=c(0,8)))

model.s <- lm(headwt ~ soil, data=cabbage)
summary(model.s)

with(cabbage,{
  plot(headwt~soil,ylim=c(0,9),xlab="type of soil",ylab="head weight",
       main="Head weight vs type of soil")
  points(as.numeric(soil),headwt)
})
legend(0.5,9,legend=c("sand = beta0","clay = beta0+beta1",
                      "loam = beta0+beta2"),
       lty=1,col=c("black","red","blue"),lwd=2)
with(model.s, abline(h=coefficients[1],col="black",lwd=2))
with(model.s, lines(c(1.5,2.5),(coefficients[1]+coefficients[2])*c(1,1),
                    col="red",lwd=2))
with(model.s, lines(c(2.5,3.5),(coefficients[1]+coefficients[3])*c(1,1),
                    col="blue",lwd=2))
with(model.s, lines(c(1.5,1.5),coefficients[1]+c(0,coefficients[2]),
                    col="red",lty=2,lwd=2))
legend(1.6,9,legend="beta1 = clay-sand", lty=2,col="red",lwd=2)
with(model.s, lines(c(2.5,2.5),coefficients[1]+c(0,coefficients[3]),
                    col="blue",lty=2,lwd=2))
legend(2.5,7,legend="beta2 = loam-sand", lty=2,col="blue",lwd=2)

### add fertilizer
model.sf <- lm(headwt~soil+fertilize,data=cabbage)     
summary(model.sf)

with(cabbage,plot(headwt ~ fertilize,ylim=c(0,9),xlab="fertilizer (x)",
                  main="Head weight vs amount of fertilizer"))
with(subset(cabbage,soil=="clay"),points(fertilize,headwt,col="red",pch=19))
with(subset(cabbage,soil=="loam"),points(fertilize,headwt,col="blue",pch=8))
with(model.sf, abline(a=coefficients[1],b=coefficients["fertilize"],
                      col="black",lwd=2))
with(model.sf, abline(a=coefficients[1]+coefficients[2],
                      b=coefficients["fertilize"],col="red",lwd=2))
with(model.sf, abline(a=coefficients[1]+coefficients[3],
                      b=coefficients["fertilize"],col="blue",lwd=2))
legend(10,9,c("sand: beta0+beta3*x","clay: beta0+beta1+beta3*x",
              "loam: beta0+beta2+beta3*x"),
       col=c("black","red","blue"),lty=1,lwd=2)
x0 <- data.frame(soil=c("sand","clay"),fertilize=c(35,35))
lines(c(35,35),predict(model.sf,x0),lty=2,col="red",lwd=2)
x0 <- data.frame(soil=c("sand","loam"),fertilize=c(37,37))
lines(c(37,37),predict(model.sf,x0),lty=2,col="blue",lwd=2)
legend(35,9,c("beta1","beta2"),col=c("red","blue"),lty=2,lwd=2)

### add interactions
model.i <- lm(headwt~soil*fertilize,data=cabbage)     
summary(model.i)

with(cabbage,plot(headwt ~ fertilize,ylim=c(0,9),xlab="fertilizer (x)",
                  main="Head weight vs amount of fertilizer, with interaction"))
with(subset(cabbage,soil=="clay"),points(fertilize,headwt,col="red",pch=19))
with(subset(cabbage,soil=="loam"),points(fertilize,headwt,col="blue",pch=8))
with(model.i, abline(a=coefficients[1],b=coefficients["fertilize"],
                     col="black",lwd=2))
with(model.i, abline(a=coefficients[1]+coefficients[2],
                     b=coefficients["fertilize"]+coefficients[5],
                     col="red",lwd=2))
with(model.i, abline(a=coefficients[1]+coefficients[3],
                     b=coefficients["fertilize"]+coefficients[6],
                     col="blue",lwd=2))
legend(10,9,c("sand: beta0+beta3*x","clay: beta0+beta1+(beta3+beta4)*x",
              "loam: beta0+beta2+(beta3+beta5)*x"),
       col=c("black","red","blue"),lty=1,lwd=2)

## ANOVA ##

# Global F-test: is three soil categories better than nothing?
summary(model.s)
anova(model.s)
qf(1-0.05,2,27)
# Yes, they are
summary(model.s)
# ... but loam is not significantly different from sand so we might only
# need two categories: sand/loam and clay

# Partial F-test: do we need the interactions?
summary(model.i)$sigma^2
anova(model.sf)
anova(model.i)
anova(model.sf,model.i)
qf(1-0.05,2,30-6)
# No, we don't