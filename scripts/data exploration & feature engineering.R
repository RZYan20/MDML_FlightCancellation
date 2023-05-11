#### III. Data Exploration & Feature Engineering####
library(tidyverse)
library(lubridate)
library(timeDate)
library(skimr)
# Import cleaned data
flight_data <- read_csv("data/flight_data.csv")

# Recoding predictors
flight_data <- flight_data %>%
  mutate(day_of_week = factor(day_of_week)) %>% 
  mutate(day_of_month = factor(day_of_month)) %>% 
  mutate(quarter = factor(quarter)) %>% 
  mutate(month = factor(month))
flight_data$cancellation_code <- recode(flight_data$cancellation_code, 
                                        "A" = "carrier",
                                        "B" = "weather",
                                        "C" = "nas",
                                        "D" = "security")
flight_data$fl_date <- as.Date(flight_data$fl_date, format = "%m/%d/%Y")

# Checking on missing values
skim(flight_data)

#### Cancellation Reasons ####
# Cancellation Reasons by airlines
month_reason <- flight_data %>%
  drop_na(cancellation_code) %>%
  group_by(month, cancellation_code) %>%
  summarize(cancel_count = n())
ggplot(month_reason, aes(x = month, y = cancel_count, fill = cancellation_code)) +
  geom_col(position = "dodge") +
  labs(title = "Monthly Cancellation Reasons", x = "Month", y = "Cancellation Count", fill = "Cancellation Reason")
ggsave("figures/Monthly Cancellation Reason.png")


#### Day and Time of Flight ####
## Day of Week - Cancellation Rate Chart 
dayofweek_rate <- flight_data %>%
  group_by(day_of_week) %>%
  summarize(cancelled_rate = mean(cancelled)) %>% 
  arrange(desc(cancelled_rate))
ggplot(data = dayofweek_rate, aes(x = day_of_week, y = cancelled_rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Day of Week Cancellation Rate") +
  xlab("Day of Week") +
  ylab("Cancellation Rate") 
ggsave("figures/Day of Week Cancellation Rate.png")
# Creating weekend indicator
flight_data <- flight_data %>% mutate(weekend = ifelse(day_of_week %in% c(6, 7), 1, 0))

## Day of Month - Cancellation Rate Chart 
flight_data <- flight_data %>%
  mutate(day_of_month = factor(day_of_month))
dayofmonth_rate <- flight_data %>%
  group_by(day_of_month) %>%
  summarize(cancelled_rate = mean(cancelled)) %>% 
  arrange(desc(cancelled_rate))
ggplot(data = dayofmonth_rate, aes(x = day_of_month, y = cancelled_rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Day of Month Cancellation Rate") +
  xlab("Day of Month") +
  ylab("Cancellation Rate") 
ggsave("figures/Day of Month Cancellation Rate.png")

# Creating holiday indicator
holidays17 <- c(as.Date(USNewYearsDay(2017)), as.Date(USMLKingsBirthday(2017)), as.Date(USMemorialDay(2017)),
              as.Date(USJuneteenthNationalIndependenceDay(2017)), as.Date(USIndependenceDay(2017)), as.Date(USLaborDay(2017)),
              as.Date(USVeteransDay(2017)), as.Date(USThanksgivingDay(2017)), as.Date(USChristmasDay(2017)))
holidays18 <- c(as.Date(USNewYearsDay(2018)), as.Date(USMLKingsBirthday(2018)), as.Date(USMemorialDay(2018)),
                as.Date(USJuneteenthNationalIndependenceDay(2018)), as.Date(USIndependenceDay(2018)), as.Date(USLaborDay(2018)),
                as.Date(USVeteransDay(2018)), as.Date(USThanksgivingDay(2018)), as.Date(USChristmasDay(2018)), as.Date(USWashingtonsBirthday(2018)))
holidays19 <- c(as.Date(USNewYearsDay(2019)), as.Date(USMLKingsBirthday(2019)), as.Date(USMemorialDay(2019)),
                as.Date(USJuneteenthNationalIndependenceDay(2019)), as.Date(USIndependenceDay(2019)), as.Date(USLaborDay(2019)),
                as.Date(USVeteransDay(2019)), as.Date(USThanksgivingDay(2019)), as.Date(USChristmasDay(2019)), as.Date(USWashingtonsBirthday(2019)))
holidays21 <- c(as.Date(USNewYearsDay(2021)), as.Date(USMLKingsBirthday(2021)), as.Date(USMemorialDay(2021)),
                as.Date(USJuneteenthNationalIndependenceDay(2021)), as.Date(USIndependenceDay(2021)), as.Date(USLaborDay(2021)),
                as.Date(USVeteransDay(2021)), as.Date(USThanksgivingDay(2021)), as.Date(USChristmasDay(2021)), as.Date(USWashingtonsBirthday(2021)))
holidays22 <- c(as.Date(USNewYearsDay(2022)), as.Date(USMLKingsBirthday(2022)), as.Date(USMemorialDay(2022)),
                as.Date(USJuneteenthNationalIndependenceDay(2022)), as.Date(USIndependenceDay(2022)), as.Date(USLaborDay(2022)),
                as.Date(USVeteransDay(2022)), as.Date(USThanksgivingDay(2022)), as.Date(USChristmasDay(2022)), as.Date(USWashingtonsBirthday(2022)))
holidays <- c(holidays17, holidays18, holidays19, holidays21, holidays22)

flight_data$holidays <- ifelse(flight_data$fl_date %in% holidays, 1, 0)

## Month in Year - Cancellation Rate Chart 
month_rate <- flight_data %>%
  group_by(month) %>%
  summarize(cancelled_rate = mean(cancelled)) %>% 
  arrange(desc(cancelled_rate))
ggplot(data = month_rate, aes(x = month, y = cancelled_rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Cancellation Rate by Month") +
  xlab("Month") +
  ylab("Cancellation Rate") 
ggsave("figures/Cancellation Rate by Month.png")

## Departure Time 
flight_data <- flight_data %>%
  mutate(dep_time_blk = str_extract(dep_time, "^\\d{2}"))
flight_data$dep_time_blk <- as.numeric(flight_data$dep_time_blk)
flight_data <- flight_data %>%
  mutate(dep_time_blk = cut(dep_time_blk, breaks = 6, labels = FALSE, include.lowest = TRUE))


#### Airlines ####
# Cancellation number - airline chart 
airline_num <- flight_data %>%
  group_by(airline) %>%
  summarize(cancelled_number = sum(cancelled)) %>% 
  arrange(desc(cancelled_number))
ggplot(data = airline_num, aes(x = reorder(airline, desc(cancelled_number)), y = cancelled_number)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Airline Cancellation Number") +
  xlab("Airlines") +
  ylab("Cancellation Number") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("figures/Airline Cancellation Number.png")

# Cancellation rate - airline chart 
airline_rate <- flight_data %>%
  group_by(airline) %>%
  summarize(cancelled_rate = mean(cancelled)) %>% 
  arrange(desc(cancelled_rate))
ggplot(data = airline_rate, aes(x = reorder(airline, desc(cancelled_rate)), y = cancelled_rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Airline Cancellation Rate") +
  xlab("Airlines") +
  ylab("Cancellation Rate") +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave("figures/Airline Cancellation Rate.png")

#### Delays ####
# Create delay rate variable
flight_data$dep_del15 <- replace(flight_data$dep_del15, is.na(flight_data$dep_del15), 0)
delay_rate <- flight_data %>%
  group_by(fl_date, airline) %>%
  summarize(delay_rate = mean(dep_del15==1))
flight_data <- flight_data %>%
  left_join(delay_rate, by = c("fl_date", "airline"))
# Create cancellation rate variable
daily_cancellation_rates <- flight_data %>%
  group_by(fl_date, airline) %>%
  summarize(cancellation_rate = mean(cancelled))
flight_data <- flight_data %>%
  left_join(daily_cancellation_rates, by = c("fl_date", "airline"))
# Calculating correlations and p value
correlation_result <- cor.test(flight_data$delay_rate, flight_data$cancellation_rate)
p_value <- correlation_result$p.value

# Creating number of flights each day by airline
flight_data <- flight_data %>%
  group_by(fl_date, airline) %>%
  mutate(num_flight = n()) %>%
  ungroup()

#### Merge Scraped Data ####
flight_score$score <- as.numeric(flight_score$score)
flight_score_mean$month <- as.factor(flight_score_mean$month)
flight_score$year <- as.factor(flight_score$year)

merge_flight_data <- flight_data %>%
  left_join(flight_score_mean, by = c("month", "year", "airline"))

# replace missing score by quarterly mean score
merge_flight_data$mean_score <- ifelse(is.na(merge_flight_data$mean_score),
                                       flight_score_quarterly_mean$quarterly_mean_score[match(paste(merge_flight_data$quarter, merge_flight_data$year, merge_flight_data$airline), 
                                                                                    paste(flight_score_quarterly_mean$quarter, flight_score_quarterly_mean$year, flight_score_quarterly_mean$airline))],
                                       merge_flight_data$mean_score)
flight_data <- merge_flight_data


# Saving the dataset
write.csv(flight_data, file = "data/fl_model_data.csv", row.names = FALSE)


