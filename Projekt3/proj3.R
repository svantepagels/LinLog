# Load data
library(lubridate)
library(httr)
library(jsonlite)
library(dplyr)


###### Load Picadeli data ######
picadeli_18 <- readRDS(file = "picadeli_data_18.rds")

dataset <- picadeli_18 %>%
  filter(country_code == 10) %>%
  filter(customer_id == 2812) %>%
  mutate(date = as.Date(ymd_hms(time_added))) %>%
  select(customer_id, product_id, product_weight, product_type_web, date) %>%
  group_by(date, product_type_web) %>%
  summarise(weight = sum(as.numeric(product_weight)))
  
# Load new picadeli data
test <- trays %>% filter(customer_id == 2812) %>%
  arrange(time_added)

###### Load SMHI API data ######
# Göteborg 71420
# Medelvärde lufttemp per dygn:  2
# Nederbördsmängd per dygn : 5

# Historical values is in gbg.txt


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

###### Load SMHI API data ######
final <- full_join(dataset, temp_dt, by = c("date"))
final <- full_join(final, rain_dt, by = c("date"))
final <- final[complete.cases(final), ]
head(final)

final <- final %>% filter(product_type_web == "Salad Proteins")

plot(final$rain, final$weight)
