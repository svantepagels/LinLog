# Load data
library(lubridate)
library("httr")
library("jsonlite")

# One store
dataset <- db_france %>%
  filter(country_code == 10) %>%
  filter(customer_id == 2812) %>%
  mutate(date = as.Date(ymd_hms(time_added))) %>%
  select(customer_id, product_id, product_weight, product_type_web, weight, date) %>%
  group_by(date) %>% # OBS: Ändra här för att groupa
  summarise(weight = sum(as.numeric(product_weight)))
  
# SMHI API
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

# Join Picadeli data with SMHI
final <- full_join(dataset, temp_dt, by = c("date"))
final <- final[complete.cases(final), ]
head(final)

plot(final$temp, final$weight)
