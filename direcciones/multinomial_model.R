# models for predicting the department  based on the address
source('./direcciones/preprocess.R')
library(rsample)
library(recipes)
library(textrecipes)
library(janitor)
library(parsnip)
library(workflows)

address_data <- preprocess_original_file()

data <- address_data %>% transmute(Id = row_number(), Department = as.factor(Department), Address = paste(Address1,Address2))

recipe <- data %>% recipe(Department ~ .) %>% step_tokenize(Address) %>% 
          update_role(Id, new_role = "ID") %>%
          step_stopwords(Address, language = "es") %>%
          step_tokenfilter(Address) %>% step_tf(Address) 

juice(prep(recipe)) %>% View()
