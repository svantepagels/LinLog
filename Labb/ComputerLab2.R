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
model <- lm(TotalSleep~Danger.medium+Danger.high, data=sleep)
