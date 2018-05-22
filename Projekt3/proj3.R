# Load data
library(lubridate)
library(httr)
library(jsonlite)
library(dplyr)
library(data.table)

###### Load Picadeli data ######
picadeli_18 <- readRDS(file = "picadeli_data_18.rds")

dataset_18 <- picadeli_18 %>%
  filter(country_code == 10) %>%
  filter(customer_id == 2812) %>%
  mutate(date = as.Date(ymd_hms(time_added))) %>%
  select(customer_id, product_id, product_weight, product_type_web, date) %>%
  group_by(date, product_type_web) %>%
  summarise(weight = sum(as.numeric(product_weight)))
  
head(dataset_18)

# Load new picadeli data
data_16_17 <- trays %>% filter(customer_id == 2812) %>%
  arrange(time_added) %>%
  mutate(date = as.Date(time_added)) %>% 
  select(customer_id, product_id, date)

product_desc <- readRDS(file = "product_descriptions_table.rds")

product_desc <- product_desc %>% select(product_id, weight, product_type_web)
data_16_17 <- left_join(data_16_17, product_desc, join_by = c("product_id"))
data_16_17 <- data_16_17 %>% 
  ungroup() %>%
  group_by(date, product_type_web) %>%
  summarise(weight = sum(weight)) %>%
  select(date, product_type_web, weight)

final_pic_type <- rbind(dataset_18, data_16_17) %>%
  arrange(date)
  
final_pic_type <- final_pic_type %>%
  mutate(year = year(date),
         weekday = wday(date)) %>%  # 2 = Monday, 1= sunday
  mutate(weekend = ifelse(weekday == 1 || weekday == 7, TRUE, FALSE)) %>%
  mutate(month = month(date), 
         quarter = quarter(date))

View(final_pic_type)

final_pic <- final_pic_type %>%
  group_by(date, year, month, weekday, quarter, weekend) %>%
  summarise(weight = sum(weight))

###### 2, 5, 20, 10
raw.result <- GET("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/2/station/71420/period/latest-months/data.json")
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)
temp_dt <- this.content$value
head(temp_dt)
temp_dt <- temp_dt %>% 
  mutate(date = as.Date(ref), 
         temp = value) %>%
  select(date, temp)

raw.result <- GET("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/5/station/71420/period/latest-months/data.json")
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)
rain_dt <- this.content$value

rain_dt <- rain_dt %>% 
  mutate(date = as.Date(ref), 
         rain = as.numeric(value)) %>%
  select(date, rain)

raw.result <- GET("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/20/station/71420/period/latest-months/data.json")
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)
max_temp_dt <- this.content$value

max_temp_dt <- max_temp_dt %>% 
  mutate(date = as.Date(ref), 
         max_temp = as.numeric(value))
max_temp_dt <- max_temp_dt[, c("date", "value")]

################# Read old values for temp, rain, max_temp, sun and merge
temp_old <- read.delim("average_temp.txt", sep=',') # id=2
rain_old <- read.delim("rain_day.txt", sep=',') # id=5
#max_temp_old <- read.delim("max_temp.txt", ",") # id=20
#sun <- read.delim("sun.txt", sep=',')

# Merge
head(temp_dt)
temp_dt <- temp_dt %>% 
  mutate(avg_temp = temp) %>%
  select(date, avg_temp)

temp_old <- temp_old %>% mutate(date = as.Date(date))
avg_temp <- rbind(temp_old, temp_dt)

rain_old <- rain_old %>% mutate(date = as.Date(date))

rain <- rbind(rain_old, rain_dt)
avg_temp <- rbind(temp_old, temp_dt)
  
#TODO: Sun + maxtemp
#sun <- read.delim("sun.txt", sep = ",") # id=10

rain <- subset(rain, date > "2016-01-01") 
avg_temp <- subset(avg_temp, date > "2016-01-01")

###### Merge SMHI API data with picadli data ######
final <- full_join(final_pic_type, rain, by = c("date"))
final <- full_join(final, avg_temp, by = c("date"))
final <- final[complete.cases(final), ]


final <- final %>%
  mutate(year = year(date),
         weekday = wday(date)) %>%  # 2 = Monday, 1= sunday
  mutate(weekend = ifelse(weekday == 1 || weekday == 7, TRUE, FALSE)) %>%
  mutate(month = month(date), 
         quarter = quarter(date))


final_date <- final %>%
  group_by(date, rain, avg_temp) %>%
  summarise(weight = sum(weight)) %>%
  mutate(year = year(date),
         weekday = wday(date)) %>%  # 2 = Monday, 1= sunday
  mutate(weekend = ifelse(weekday == 1 || weekday == 7, TRUE, FALSE)) %>%
  mutate(month = month(date), 
         quarter = quarter(date))

final <- write.csv(final, "final.csv")
final_date <- write.csv(final. "final.csv")

################## TO ANNA 2018 ----
final <- full_join(dataset_18, temp_dt, by = c("date"))
final <- full_join(final, rain_dt, by = c("date"))
final <- full_join(final, max_temp_dt, by = c("date"))

final <- final %>%
  mutate(year = year(date),
         weekday = wday(date)) %>%  # 2 = Monday, 1= sunday
  mutate(weekend = ifelse(weekday == 1 || weekday == 7, TRUE, FALSE)) %>%
  mutate(month = month(date), 
         quarter = quarter(date))

write.csv(final, "data_examlpe.csv")




###### SLASK######----
# Göteborg 71420
# Medelvärde lufttemp per dygn:  2
# Nederbördsmängd per dygn : 5

# List
raw.result <- GET("https://opendata-download-metobs.smhi.se/api/version/latest.json")
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)
a <- this.content$resource$key
b <- this.content$resource$summary
c <- this.content$resource$title
dt <- as.data.frame(cbind(a,b, c)) %>% arrange(a)
View(dt)
temp_dt <- this.content$value

# Historical values is in gbg.txt
raw.result <- GET("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/2/station/71420/period/corrected-archive.json")
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)
names(this.raw.content)
temp_dt <- this.content$value

# Last 4 months is here:

# Temp i Göteborg
raw.result <- GET("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/2/station/71420/period/latest-months/data.json")
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)
names(this.raw.content)
temp_dt <- this.content$value

temp_dt <- temp_dt %>% 
  mutate(date = as.Date(ref), 
         temp = value) %>%
  select(date, temp)

raw.result <- GET("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/5/station/71420/period/latest-months/data.json")
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)
names(this.raw.content)
rain_dt <- this.content$value

rain_dt <- rain_dt %>% 
  mutate(date = as.Date(ref), 
         rain = value) %>%
  select(date, rain)

