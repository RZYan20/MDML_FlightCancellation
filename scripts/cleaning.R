#### I.Reading and Cleaning ####
library(tidyverse)
library(lubridate)

# Create months and years vector
months <- c(1:12)
years <- c('2017', '2018', '2019', '2021', '2022')
# Import & combine datasets
flight_data <- data.frame()
for(year in years){
for(month in months){
  filename <- paste0('../Downloads/data/', year,'.', month, '.csv')
  this_data <- read.csv(filename)
  flight_data <- rbind(flight_data, this_data)
}}

# Change all variable names into lowercase
flight_data <- flight_data %>% rename_all(tolower)

# Drop irrelevant variables
flight_data <- flight_data %>% 
  select(c(-origin_city_name, -origin_airport_id, -origin_wac, -dest_airport_id, -dest_city_name, -dest_wac, 
           -crs_dep_time,-dep_time, -dep_delay_new, -dep_delay_group, -arr_time, 
           -arr_delay_new, -arr_del15, -arr_delay_group, -arr_time_blk, -flights, -crs_arr_time, 
           -carrier_delay, -weather_delay, -nas_delay, -security_delay, -late_aircraft_delay
  ))

# Renaming variables
flight_data <- flight_data %>% 
  rename(airline = op_unique_carrier, fl_number = op_carrier_fl_num, dep_time = dep_time_blk)

## Choosing target airport of prediction by cancellation frequency
# Cancellation number - airport chart 
airport_num <- flight_data %>%
  group_by(origin) %>%
  summarize(cancelled_number = sum(cancelled)) %>% 
  arrange(desc(cancelled_number))
top_airport_num <- airport_num %>% 
  arrange(desc(cancelled_number)) %>% 
  top_n(10, cancelled_number)
ggplot(data = top_airport_num, aes(x = reorder(origin, cancelled_number), y = cancelled_number)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Top 10 Cancellation Number Airports") +
  xlab("Airport") +
  ylab("Cancellation Number") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()
ggsave("figures/Top 10 Cancellation Number Airports.png")

# Cancellation rate - airport chart 
airport_rate <- flight_data %>%
  group_by(origin) %>%
  summarize(cancelled_rate = mean(cancelled)) %>% 
  arrange(desc(cancelled_rate))
top_airport_rates <- airport_rate %>% 
  arrange(desc(cancelled_rate)) %>% 
  top_n(30, cancelled_rate)
ggplot(data = top_airport_rates, aes(x = reorder(origin, cancelled_rate), y = cancelled_rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Top 30 Cancellation Rate Airports") +
  xlab("Airport") +
  ylab("Cancellation Rate") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

# Restricting dataset to LGA origin/destination only
flight_data <- flight_data %>%
  filter(origin == "LGA")


# Saving the dataset
write.csv(flight_data, file = "data/flight_data.csv")

