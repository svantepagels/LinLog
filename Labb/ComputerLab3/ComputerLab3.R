library(dplyr)
cdi <- read.delim("CDI.txt")
cdi$region <- factor(cdi$region, levels=c(1,2,3,4),
                     labels=c("Northeast","Midwest","South","West"))
cdi$phys1000 <- 1000*cdi$phys/cdi$popul
cdi$crm1000 <- 1000*cdi$crimes/cdi$popul

# Plot phys1000 against each of percapitaincome, crm1000 and pop65plus. Also plot
# log(phys1000) against the others. Does is seem reasonable to take the log?
with(cdi, plot(phys1000 ~ percapitaincome, 
                xlab="Per Capita income", ylab="Physicians per 1000"))

with(cdi, plot(log(phys1000) ~ percapitaincome, 
               xlab="Per Capita income", ylab="log Physicians per 1000"))

with(cdi, plot(phys1000 ~ crm1000, 
               xlab="Crime rate", ylab="Physicians per 1000"))

with(cdi, plot(log(phys1000) ~ crm1000, 
               xlab="Crime rate", ylab="log Physicians per 1000"))

with(cdi, plot(phys1000 ~ pop65plus, 
               xlab="65 plus pop", ylab="Physicians per 1000"))

with(cdi, plot(log(phys1000) ~ pop65plus, 
               xlab="65 plus pop", ylab="log Physicians per 1000"))

# Yes it seems reasonable to take the log since the dependcy becomes more linear

#Use log(phys1000) as dependent variable and fit all 2p = 2^3 = 8 possible subsets of the three
#x-variables. We assume that we do not need any interaction terms. Calculate R^2_adj and BIC for
#each model. Which model is ???best????

model.1 = lm(log(phys1000) ~ 1, data=cdi)
model.2 = lm(log(phys1000) ~ percapitaincome, data=cdi) 
model.3 = lm(log(phys1000) ~ percapitaincome+crm1000, data=cdi) 
model.4 = lm(log(phys1000) ~ percapitaincome+crm1000+pop65plus, data=cdi) 
model.5 = lm(log(phys1000) ~ crm1000, data=cdi) 
model.6 = lm(log(phys1000) ~ crm1000+pop65plus, data=cdi) 
model.7 = lm(log(phys1000) ~ pop65plus, data=cdi) 
model.8 = lm(log(phys1000) ~ pop65plus+percapitaincome, data=cdi) 

# R-squared adjusted
r2adj = c(summary(model.1)$adj.r.squared, 
          summary(model.2)$adj.r.squared,
          summary(model.3)$adj.r.squared, 
          summary(model.4)$adj.r.squared,
          summary(model.5)$adj.r.squared,
          summary(model.6)$adj.r.squared,
          summary(model.7)$adj.r.squared,
          summary(model.8)$adj.r.squared)

# BIC 
bic <- c(AIC(model.1,model.2,model.3, model.4, model.5,
            model.6, model.7, model.8, k=log(nrow(cdi))))

crit.models <- data.frame(r2adj, bic)
plot(crit.models$r2adj, type='l', xlab='Model', ylab='R2 adj', col='red')
plot(crit.models$AIC, type='l', xlab='Model', ylab='BIC', col='blue')

# Model 3 with per capita income and crime rate seems to be the best overall

#Use the largest model and plot the leverage, vii, both in order (against i) and against each of the
#three x-variables. Is there any observation with a dangerously large leverage? Try to identify in
#which variable the problem lies. Find out which county this is.

v <- hatvalues(model.4) # leverage
plot(v, xlab='i', ylab='Leverage')
plot(v~cdi$percapitaincome, xlab='Per capita income', ylab='Leverage')
plot(v~cdi$crm1000, xlab='Crime rate', ylab='Leverage')
plot(v~cdi$pop65plus, xlab='65 plus pop', ylab='Leverage')


cdi[which(v>0.15),2] 

# Kings county seems to be the outlier since their crime rate is insane

#Calculate the studentized residuals, r???i, and plot them against each of the three x-variables. Also
#plot them against the predicted values, Y??i. Any problems?

r_stud <- rstudent(model.4) # studentised residuals
plot(r_stud, xlab='i', ylab='Studentised res')
abline(h=0)
abline(h=c(-2,2),col="red")
plot(r_stud~cdi$percapitaincome, xlab='Per capita income', ylab='Studentised res')
abline(h=0)
abline(h=c(-2,2),col="red")
plot(r_stud~cdi$crm1000, xlab='Crime rate', ylab='Studentised res')
abline(h=0)
abline(h=c(-2,2),col="red")
plot(r_stud~cdi$pop65plus, xlab='65 plus pop', ylab='Studentised res')
abline(h=0)
abline(h=c(-2,2),col="red")
plot(r_stud~model.4$fit, xlab='Fitted values', ylab='Studentised res')
abline(h=0)
abline(h=c(-2,2),col="red")

# Seems like there is some structural problem in the model since many studentised residuals 
# are very high. Also, the King's county data point seems to be very low. 

#Look at the s(i) used in the studentized residuals. Which county produced the largest decrease
#when left out? What about the problematic one from (c)?

infl <- influence(model.4) # will calculate basic influence measures
s_i <- infl$sigma #a vector whose i-th element contains the estimate of the
#residual standard deviation obtained when the i-th case is dropped from the regression

plot(s_i) 
cdi[which(s_i<0.438),2]

# The little bastard of King's county is one of the problematic points. 
# Olmsted county is also a bitch. 

#Calculate Cook???s distance, Di and plot it against the x-variables. Any problems? What about the
#problematic counties from (c) and (e)?

D <- cooks.distance(model.4)
plot(D)
abline(h=4/nrow(cdi), col='red')
abline(h=1, col='green')
cdi[which(D>0.8),2] # The sucker King's county is there again, Olmsted seems cool. 
sum(count(which(D>4/nrow(cdi)))$freq) # 27 little try-hards have Cook's distance above 4/n

#Calculate the DFBETAj and plot each one of them. Which of the ??-parameters have been most
#influenced by the problematic counties?

dfb <- dfbetas(model.4)
plot(dfb[,1]) # King's increase the intercept strongly
points(6,dfb[6,1],col="red",pch=19)
points(418,dfb[418,1],col="green",pch=19)
plot(dfb[,2]) 
points(6,dfb[6,2],col="red",pch=19)
points(418,dfb[418,2],col="green",pch=19)
plot(dfb[,3]) # Kings's decrease crime rate strongly
points(6,dfb[6,3],col="red",pch=19)
points(418,dfb[418,3],col="green",pch=19)
plot(dfb[,4])
points(6,dfb[6,4],col="red",pch=19)
points(418,dfb[418,4],col="green",pch=19)



####################################################################################################
#                                        Redoing everything                                        #
####################################################################################################

#Leave out the problematic county from (c) and re-fit the largest model. Then redo (c)???(g) and
#see what happens.

cdi <- cdi %>%
  filter(id != 6)


#Use log(phys1000) as dependent variable and fit all 2p = 2^3 = 8 possible subsets of the three
#x-variables. We assume that we do not need any interaction terms. Calculate R^2_adj and BIC for
#each model. Which model is ???best????

model.4 = lm(log(phys1000) ~ percapitaincome+crm1000+pop65plus, data=cdi) 

# R-squared adjusted
r2adj = c(summary(model.4)$adj.r.squared)

# BIC 
bic <- c(AIC(model.4, k=log(nrow(cdi))))

crit.models <- data.frame(r2adj, bic)
plot(crit.models$r2adj, type='p', xlab='Model', ylab='R2 adj', col='red')
plot(crit.models$AIC, type='p', xlab='Model', ylab='BIC', col='blue')

#Use the largest model and plot the leverage, vii, both in order (against i) and against each of the
#three x-variables. Is there any observation with a dangerously large leverage? Try to identify in
#which variable the problem lies. Find out which county this is.

v <- hatvalues(model.4) # leverage
plot(v, xlab='i', ylab='Leverage')
plot(v~cdi$percapitaincome, xlab='Per capita income', ylab='Leverage')
plot(v~cdi$crm1000, xlab='Crime rate', ylab='Leverage')
plot(v~cdi$pop65plus, xlab='65 plus pop', ylab='Leverage')


cdi[which(v>0.15),2] 

# Kings county seems to be the outlier since their crime rate is insane

#Calculate the studentized residuals, r???i, and plot them against each of the three x-variables. Also
#plot them against the predicted values, Y??i. Any problems?

r_stud <- rstudent(model.4) # studentised residuals
plot(r_stud, xlab='i', ylab='Studentised res')
abline(h=0)
abline(h=c(-2,2),col="red")
plot(r_stud~cdi$percapitaincome, xlab='Per capita income', ylab='Studentised res')
abline(h=0)
abline(h=c(-2,2),col="red")
plot(r_stud~cdi$crm1000, xlab='Crime rate', ylab='Studentised res')
abline(h=0)
abline(h=c(-2,2),col="red")
plot(r_stud~cdi$pop65plus, xlab='65 plus pop', ylab='Studentised res')
abline(h=0)
abline(h=c(-2,2),col="red")
plot(r_stud~model.4$fit, xlab='Fitted values', ylab='Studentised res')
abline(h=0)
abline(h=c(-2,2),col="red")

# Seems like there is some structural problem in the model since many studentised residuals 
# are very high. Also, the King's county data point seems to be very low. 

#Look at the s(i) used in the studentized residuals. Which county produced the largest decrease
#when left out? What about the problematic one from (c)?

infl <- influence(model.4) # will calculate basic influence measures
s_i <- infl$sigma #a vector whose i-th element contains the estimate of the
#residual standard deviation obtained when the i-th case is dropped from the regression

plot(s_i) 
points(417,s_i[417],col="green",pch=19)

# The little bastard of King's county is one of the problematic points. 
# Olmsted county is also a bitch. 

#Calculate Cook???s distance, Di and plot it against the x-variables. Any problems? What about the
#problematic counties from (c) and (e)?

D <- cooks.distance(model.4)
plot(D)
abline(h=4/nrow(cdi), col='red')
abline(h=1, col='green')
cdi[which(D>0.8),2] # The sucker King's county is there again, Olmsted seems cool. 
length(which(D>4/nrow(cdi))) # 27 little try-hards have Cook's distance above 4/n

#Calculate the DFBETAj and plot each one of them. Which of the ??-parameters have been most
#influenced by the problematic counties?

dfb <- dfbetas(model.4)
plot(dfb[,1]) # King's increase the intercept strongly
points(417,dfb[417,1],col="green",pch=19)
plot(dfb[,2]) 
points(417,dfb[417,2],col="green",pch=19)
plot(dfb[,3]) # Kings's decrease crime rate strongly
points(417,dfb[417,3],col="green",pch=19)
plot(dfb[,4])
points(417,dfb[417,4],col="green",pch=19)

# Both r^2 adjusted and BIC scores are better than for the best model in the first iteration. 