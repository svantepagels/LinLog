library(dplyr)

# Load data
data <- read.csv("final.csv")  # read csv file 
data$X <- NULL
max_temp <- read.delim("max_temp.txt", sep=',')
data <- left_join(data, max_temp, join_by = c("date"))

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

############################ Data investigation ############################


# Plot different sales over time
with(data %>% filter(product_type_web=="Salad Accompaniments"),
     plot(weight, type = 'l'))

with(data %>% group_by(quarter) %>% summarise(weight),
     plot(weight))

# See data class of column
sapply(my.data, class)

