library(dplyr)

# Load data
data <- read.csv("final.csv")  # read csv file 
data$X <- NULL
max_temp <- read.delim("max_temp.txt", sep=',')
data <- left_join(data, max_temp, join_by = c("date"))
sun <- read.delim("sun.txt", sep=',')
sun <- sun %>% group_by(date) %>%
  summarise(sun_time=sum(sun_time))
data <- left_join(data, sun, join_by = c("date"))
############################ Factor data that are categorical ############################
# Product type
data$product_type_web <- factor(data$product_type_web,
                                levels=c("Salad Accompaniments","Salad Cheese",
                                         "Salad Fruit","Salad Lettuce", "Salad Pasta & Grains",
                                         "Salad Proteins", "Salad Toppings", "Salad Vegetables"),
                                labels=c("Salad Accompaniments","Salad Cheese",
                                         "Salad Fruit","Salad Lettuce", "Salad Pasta & Grains",
                                         "Salad Proteins", "Salad Toppings", "Salad Vegetables"))
# If rain or not
data$rain_factor <- data$rain > 0
data$rain_factor <- factor(data$rain_factor, 
                           levels = c(TRUE, FALSE),
                           labels=c("rain", "no_rain"))

# If sunny or not (sunny = 36000 seconds sun per day)
data$sun_time_factor <- data$sun_time > 36000
data$sun_time_factor <- factor(data$sun_time_factor, 
                           levels = c(TRUE, FALSE),
                           labels=c("Sunny", "not_sunny"))
# Year
data$year <- factor(data$year,
                    labels = c("2016","2017", "2018"))
# Weekday
data$weekday <- factor(data$weekday,
                       levels = c(1:7),
                       labels = c("sunday", "monday", "tuesday", "wednesday",
                                  "thursday", "friday", "saturday"))

# Weekend
data$weekend <- factor(data$weekend, 
                       levels = c(TRUE, FALSE),
                       labels = c("weekend", "not_weekend"))

# Month
data$month <- factor(data$month, 
                     levels = c(1:12),
                     labels = c("january", "february", "march", "april",
                                "may", "june", "july", "august", "september",
                                "october", "november", "december"))
# Change so that quarters continue with Q1 2016 = 1 until Q2 2018 = 10
data[which(data$year == "2017"), 10] <- data[which(data$year == "2017"), 10] + 4 
data[which(data$year == "2018"), 10] <- data[which(data$year == "2018"), 10] + 8

# Factor the data
data$quarter <- factor(data$quarter)

# Date column to date-class
data$date <- as.Date(data$date)


############################ Create dataset with all types grouped ############################

data2_with_outliers <- data %>% 
  group_by(date, rain, avg_temp, year, weekday, weekend, month, max_temp, rain_factor, sun_time,
           sun_time_factor) %>% 
  summarise(weight=sum(weight))

# Remove outlier values over 500 kilograms per day
data2 <- data2_with_outliers[data2_with_outliers$weight<550,]

############################ Data investigation ############################


# Plot different sales over time
with(data %>% filter(product_type_web=="Salad Pasta & Grains"),
     plot(weight~date, type = 'l'))
lines(data %>% filter(product_type_web=="Salad Cheese") %>% select(date,weight), col='red')
lines(data %>% filter(product_type_web=="Salad Fruit") %>% select(date,weight), col='blue')
lines(data %>% filter(product_type_web=="Salad Lettuce") %>% select(date,weight), col='green')
lines(data %>% filter(product_type_web=="Salad Accompaniments") %>% select(date,weight), col='purple')
lines(data %>% filter(product_type_web=="Salad Proteins") %>% select(date,weight), col='pink')
lines(data %>% filter(product_type_web=="Salad Toppings") %>% select(date,weight), col='orange')
lines(data %>% filter(product_type_web=="Salad Vegetables") %>% select(date,weight), col='grey')


############################ Stepwise Model building with different sallad types ############################

# Step function
fullmodel <- lm(weight~product_type_web+rain+avg_temp+weekday+year+weekend+month+max_temp+rain_factor, sun_time,data=data)
emptymodel <- lm(weight~1, data=data)

#forwards <- step(emptymodel,
#                 scope=list(lower=formula(emptymodel),upper=formula(fullmodel)), direction="forward",
#                 k=log(nrow(data))) # Funkar inte av någon anledning

backwards <- step(fullmodel,k = log(nrow(data))) 

summary(backwards)

############################ Stepwise Model building without different sallad types ############################

# Step function
fullmodel2 <- lm(weight~rain+avg_temp+year+weekend+max_temp+rain_factor+sun_time,
                data=data2)
emptymodel2 <- lm(weight~1, data=data2)

#forwards <- step(emptymodel,
#                 scope=list(lower=formula(emptymodel),upper=formula(fullmodel)), direction="forward",
#                 k=log(nrow(data))) # Funkar inte av någon anledning

backwards2 <- step(fullmodel2,k = log(nrow(data2))) 

summary(backwards2)


############################ Weather effect ############################

model.weather <- lm(log(weight) ~ avg_temp + year + weekend + sun_time, data=data2)
summary(model.weather)

############################ Plots ############################
# Rain
with(data2, plot(weight~rain_factor))
with(data2, plot(weight~rain))

test <- lm(weight~rain_factor, data=data2)
summary(test)

test <- lm(weight~rain, data=data2)
summary(test)

# Max temp
with(data2, plot(weight~max_temp))

test <- lm(weight~max_temp, data=data2)
summary(test)

# Average temp
with(data2, plot(weight~avg_temp))

test <- lm(weight~avg_temp, data=data2)
summary(test)

# Sun time
with(data2, plot(weight~sun_time))

test <- lm(weight~sun_time, data=data2)
summary(test)

# Month and avg temp
with(data2, plot(sun_time~month))
with(data2, plot(avg_temp~month))

######################### DATA ######################### 
# Dependent variable
hist(data2_with_outliers$weight, breaks = 40, xlab = "Weight", main = "Sales")

# Problems with outliers
plot(data2_with_outliers$weight~data2_with_outliers$date, ylab="Weight", xlab="Date")
lines(rep(550,length(data2_with_outliers$date))~data2_with_outliers$date, col="red")

# Independent variables
############################################

# Average temerature
plot(data2$avg_temp~data2$month, ylab="Daily average temp", xlab="Month")
plot(data2$weight ~ data2$avg_temp, ylab="Weight", xlab="Daily average temp") 
# TODO lägg in linje
plot(log(data2$weight) ~ data2$avg_temp, ylab="Log Weight", xlab="Daily average temp") 

############################################

# Max temerature
plot(data2$max_temp~data2$month, ylab="Daily max temp", xlab="Month")
plot(data2$weight ~ data2$max_temp, ylab="Weight", xlab="Daily average temp") 
plot(log(data2$weight) ~ data2$max_temp, ylab="Log Weight", xlab="Daily average temp") 
############################################

# Sun time
plot(data2$sun_time~data2$month, ylab="Daily sun time", xlab="Month")
plot(data2$weight ~ data2$sun_time, ylab="Weight", xlab="Daily sun time") 
plot(log(data2$weight) ~ data2$sun_time, ylab="Log Weight", xlab="Daily sun time") 

# Factor sun time
plot(data2$weight ~ data2$sun_time_factor, ylab="Weight", xlab="Sunny or not") 
plot(log(data2$weight) ~ data2$sun_time_factor, ylab="Log Weight", xlab="Sunny or not") 
############################################

# Rain
plot(data2$sun_time~data2$month, ylab="Daily sun time", xlab="Month")
plot(data2$weight ~ data2$sun_time, ylab="Weight", xlab="Daily rain") 
plot(log(data2$weight) ~ data2$sun_time, ylab="Log Weight", xlab="Daily rain") 

# Categorical
plot(data2$weight ~ data2$rain_factor, ylab="Weight", xlab="Rain or not") 


############################################ Step-wise reduction ############################################ 

# Step function
fullmodel3 <- lm(weight~rain+rain_factor+avg_temp+max_temp+sun_time+sun_time_factor,
                 data=data2)
emptymodel3 <- lm(weight~1, data=data2)

forwards3 <- step(emptymodel3,
                 scope=list(lower=formula(emptymodel3),upper=formula(fullmodel3)), direction="forward",
                 k=log(nrow(data2)))

backwards3 <- step(fullmodel3,k = log(nrow(data2))) 

summary(backwards3)
summary(forwards3)

# With log weight

fullmodel3.1 <- lm(log(weight)~rain+rain_factor+avg_temp+max_temp+sun_time+sun_time_factor,
                 data=data2)
emptymodel3.1 <- lm(log(weight)~1, data=data2)

forwards3.1 <- step(emptymodel3.1,
                  scope=list(lower=formula(emptymodel3.1),upper=formula(fullmodel3.1)), direction="forward",
                  k=log(nrow(data2)))

backwards3.1 <- step(fullmodel3.1,k = log(nrow(data2))) 

summary(backwards3.1)
summary(forwards3.1)

# Conclusion: Temperature and sun_time seem significant and log weight makes things worse

################################# Adjusting for other independent variables ##################################### 

# Day of the week
plot(data2$weight ~ data2$weekday, ylab="Weight", xlab="Weekday")
plot(log(data2$weight) ~ data2$weekday, ylab="Weight", xlab="Weekday")

# Weekend or weekday
plot(data2$weight ~ data2$weekend, ylab="Weight", xlab="Weekend") 

# Year
plot(data2$weight ~ data2$year, ylab="Weight", xlab="Year") 

# Adding weekend (since more relevant when ordering) and year

# With max temp
model1 <- lm(weight~relevel(sun_time_factor, ref = "not_sunny") + max_temp + weekend + year, data=data2)
summary(model1)
# With interaction 
model1.1 <- lm(weight~relevel(sun_time_factor, ref = "not_sunny")*max_temp + weekend + year, data=data2)
summary(model1.1)

# With average temp
model2 <- lm(weight~relevel(sun_time_factor, ref = "not_sunny") + avg_temp + weekend + year, data=data2)
summary(model2)
# With interaction 
model2.1 <- lm(weight~relevel(sun_time_factor, ref = "not_sunny")*avg_temp + weekend + year, data=data2)
summary(model2.1)

AIC(model1, k=log(nrow(data2)))
AIC(model1.1, k=log(nrow(data2)))

AIC(model2, k=log(nrow(data2)))
AIC(model2.1, k=log(nrow(data2)))

# Since interaction between max_temp and sun gives strange results, we will use without interaction. 
# model1 gives slightly better AIC --> This is the one we use

################################# Problematic observations ##################################### 

# Plot residuals against time
Residuals <- summary(model1)$residuals
plot(Residuals~data2$date, xlab="Date")

# Studentized residuals
v <- hatvalues(model1) # leverage
plot(v~data2$date)
