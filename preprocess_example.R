library(tidymodels)
library(nycflights13)
library(skimr)

set.seed(11052019)

flight_data <- 
  flights %>% 
  mutate(
    arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
    arr_delay = factor(arr_delay),
    date = as.Date(time_hour)
  ) %>% 
  inner_join(weather, by = c("origin", "time_hour")) %>% 
  select(dep_time, flight, origin, dest, air_time, distance, 
         carrier, date, arr_delay, time_hour) %>% 
  na.omit %>% 
  mutate_if(is.character, as.factor)

flight_data

flight_data %>% 
  count(arr_delay) %>% 
  mutate(prop = n/sum(n))

glimpse(flight_data)

flight_data %>% 
  skimr::skim(dest, carrier) 



data_split <- initial_split(flight_data)
train_data <- training(data_split)
test_data  <- testing(data_split)

flights_rec <- recipe(arr_delay ~ ., data = train_data)  %>% update_role(flight, time_hour, new_role = "ID")%>%
               step_date(date, features = c("dow", "month")) %>%               
               step_holiday(date, holidays = timeDate::listHolidays("US")) %>% 
               step_rm(date) %>%
               step_dummy(all_nominal(), -all_outcomes()) %>% 
               step_zv(all_predictors())

lr_mod <- logistic_reg() %>% set_engine("glm")

flights_wf <- workflows::workflow() %>% add_model(lr_mod) %>% add_recipe(flights_rec)

summary(flights_wf)

flights_fit <- flights_wflow %>% fit(data = train_data)

flights_fit %>% pull_workflow_fit() %>% tidy()

flights_pred <- 
  predict(flights_fit, test_data, type = "prob") %>% 
  bind_cols(test_data %>% select(arr_delay, time_hour, flight)) 

flights_pred %>% roc_curve(truth = arr_delay, .pred_late) %>% autoplot()

flights_pred %>% roc_auc(truth = arr_delay, .pred_late) 




