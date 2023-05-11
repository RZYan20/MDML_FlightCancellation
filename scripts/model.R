#### IV. Model Building ####
library(tidyverse)
library(lubridate)
library(ROCR)
library(ranger)
library(dplyr)
library(skimr)


# Import data
model_data <- read_csv("data/fl_model_data.csv")
model_data$airline <- as.factor(model_data$airline)

# Clean NA review scores 

# create a copy of model_data with mean_score values for 9E replaced with DL since 9E own by DL
replacement_data <- model_data %>%
  filter(airline == "DL") %>%
  dplyr::select(month, year, mean_score) %>%
  rename(dl_mean_score = mean_score)%>%
  distinct()

model_data <- merge(model_data, replacement_data, by = c("month", "year"), all.x = TRUE)
model_data$mean_score[model_data$airline == "9E"] <- model_data$dl_mean_score[model_data$airline == "9E"]
model_data$dl_mean_score <- NULL

# replace OH with AA scores
replacement_data2 <- model_data %>%
  filter(airline == "AA") %>%
  dplyr::select(month, year, mean_score) %>%
  rename(aa_mean_score = mean_score)%>%
  distinct()

model_data <- merge(model_data, replacement_data2, by = c("month", "year"), all.x = TRUE)
model_data$mean_score[model_data$airline == "OH"] <- model_data$aa_mean_score[model_data$airline == "OH"]
model_data$aa_mean_score <- NULL


model_data <- model_data %>%
  mutate(mean_score = ifelse(airline == "EV", 8, mean_score)) %>%
  mutate(mean_score = ifelse(airline == "MQ", 5.2, mean_score)) %>%
  mutate(mean_score = ifelse(airline == "YV", 6.8, mean_score)) %>%
  mutate(mean_score = ifelse(airline == "YX", 6.2, mean_score)) %>%
  mutate(mean_score = ifelse(airline == "OO", 4, mean_score))



# Setting training and testing dataset
train <- model_data %>% filter(year == 2017 | year == 2018)
test_19 <- model_data %>% filter(year == 2019)
test_21 <- model_data %>% filter(year == 2021)
test_22 <- model_data %>% filter(year == 2022)
#skim(model_data)


#### Logistic Regression Model ####
## Setting model
formula_meanscore <- cancelled ~ airline + weekend + holidays + distance + delay_rate + dep_time_blk + num_flight + mean_score
formula <- cancelled ~ airline + weekend + holidays + distance + delay_rate + dep_time_blk + num_flight 
model <- glm(formula, data = train, family = 'binomial')

## Predicting Flight Cancellations 
# Test year 2019
test_19 <- test_19 %>% mutate(predicted.probability = 
                                  predict(model, test_19, type = 'response'))

test19.pred <- prediction(test_19$predicted.probability, test_19$cancelled)
test19.auc <- performance(test19.pred, "auc")
cat('the auc score is ', test19.auc@y.values[[1]], "\n")

# Test year 2021
test_21 <- test_21 %>% mutate(predicted.probability = 
                                predict(model, test_21, type = 'response'))

test21.pred <- prediction(test_21$predicted.probability, test_21$cancelled)
test21.auc <- performance(test21.pred, "auc")
cat('the auc score is ', test21.auc@y.values[[1]], "\n")

# Test year 2022
test_22 <- test_22 %>% mutate(predicted.probability = 
                                predict(model, test_22, type = 'response'))

test22.pred <- prediction(test_22$predicted.probability, test_22$cancelled)
test22.auc <- performance(test22.pred, "auc")
cat('the auc score is ', test22.auc@y.values[[1]], "\n")



#### Random Forest Model  ####
## Setting model
rf_model = ranger(formula, data=train, respect.unordered.factors = TRUE, probability = TRUE, num.trees = 1000)
## Testing and calculating auc
# Test year 2019

test_pred_prob_2 = predict(model2,test_2,type='response')
test.pred = prediction(test_pred_prob_2$predictions[,2],test_2$outcome)
test.auc = performance(test.pred, 'auc')
cat('The AUC score is', test.auc@y.values[[1]], "\n") 


rf_test19 = test_19 %>%
  select(airline, weekend, holidays, distance, delay_rate, dep_time_blk, num_flight)

rf_predictions19 = predict(rf_model,rf_test19,type='response')
rf_pred19 = prediction(rf_predictions19$predictions[,2],test_19$cancelled)
rf_auc19 = performance(rf_pred19, 'auc')
cat('The AUC score is', rf_auc19@y.values[[1]], "\n") 

# Test year 2021
rf_test21 = test_21 %>%
  select(airline, weekend, holidays, distance, delay_rate, dep_time_blk, num_flight)

rf_predictions21 = predict(rf_model,rf_test21,type='response')
rf_pred21 = prediction(rf_predictions21$predictions[,2],test_21$cancelled)
rf_auc21 = performance(rf_pred21, 'auc')
cat('The AUC score is', rf_auc21@y.values[[1]], "\n") 

# Test year 2022
rf_test22 = test_22 %>%
  select(airline, weekend, holidays, distance, delay_rate, dep_time_blk, num_flight)

rf_predictions22 = predict(rf_model,rf_test22,type='response')
rf_pred22 = prediction(rf_predictions22$predictions[,2],test_22$cancelled)
rf_auc22 = performance(rf_pred22, 'auc')
cat('The AUC score is', rf_auc22@y.values[[1]], "\n") 



# recall-at-k% plot comparing logistic and random forest model 
test_19 = test_19 %>%
  mutate(test_pp_logi = predicted.probability,
         test_pp_rf = rf_predictions19$predictions[,2])

plot_logi <- test_19 %>% 
  mutate(predict_prob = predicted.probability) %>% 
  arrange(desc(predict_prob)) %>%
  mutate(rank = row_number(),
         precision = cumsum(cancelled == 1)/rank) %>% 
  filter(rank > 100)

plot_rf = test_19 %>% arrange(test_pp_rf)

plot_rf = plot_rf %>%
  arrange(desc(test_pp_rf)) %>%
  select(cancelled,test_pp_rf) %>%
  mutate(rank = row_number(), precision = cumsum(cancelled==1)/rank)
plot_rf = plot_rf[101:nrow(plot_rf), ]

plot = ggplot() +
  geom_line(data = plot_rf,aes(x=rank,y=precision), color="blue")+
  geom_line(data = plot_logi,aes(x=rank,y=precision), color="red")

plot
ggsave("figures/performance_plot.png")
