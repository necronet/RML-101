install.packages("janitor")
install.packages("skimr")

vehicles = read.csv("vehicle_example/vehicles.csv", header = TRUE)

library(janitor)
library(skimr)
library(dplyr)
library(stringr)
library(ranger) 
library(rsample)

vehicles_clean <- janitor::clean_names(vehicles)


scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

#
vehicles_no_na <- vehicles_clean %>% 
  mutate(manufacturer = stringr::str_trim(manufacturer)) %>%
  select('year','manufacturer','model','condition','odometer','type','price')  %>% 
  filter_all(~ . != "") %>% na.omit() %>% 
  filter(odometer > 0) %>%
  mutate_if(is.character, as.factor) 

set.seed(100519)
init_split <- initial_split(vehicles_no_na, prop = .7)
training <- training(init_split)
testing  <- testing(init_split)

skimr::skim(training)

rf_1 <- ranger::ranger(formula = price ~ .,data = vehicles_no_na, num.trees = 500)

plot(rf_1)



