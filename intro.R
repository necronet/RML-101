# Exploring the tidyverse with ML
library(gapminder)
library(purrr)
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(rsample)
library(Metrics)
library(ranger)

gap_nested <- gapminder %>% group_by(country) %>% nest()
gap_unnested <- gap_nested %>% unnest(data)
identical(gap_unnested, gapminder)
gap_nested$country == "Algeria"

algeria_df <- gap_nested$data[[1]]

min(algeria_df$pop)
max(algeria_df$pop)
mean(algeria_df$pop)

pop_nested <- gap_nested %>% mutate(mean_pop = map(data, ~mean(.x$pop)))

pop_mean <- pop_nested %>% unnest(mean_pop)

# Nest multiple linear regression models
gap_models <- gap_nested %>% mutate(model = map(data, ~lm(formula = lifeExp~year, data = .x)))

algeria_model <- gap_models$model[[1]]
summary(algeria_model)

# Extract the coefficients of the algeria_model as a dataframe
tidy(algeria_model)

# Extract the statistics of the algeria_model as a dataframe
glance(algeria_model)

algeria_fitted <- augment(algeria_model)

algeria_fitted %>% ggplot(aes(x = year)) + 
  geom_point(aes(y = lifeExp)) + 
  geom_line(aes(y = .fitted), color= "red")

model_coef_nested <- gap_models %>%  mutate(coef = map(model, ~tidy(.x)))
model_coef <- model_coef_nested %>% unnest(coef)

model_coef %>% filter(term == "year") %>% ggplot(aes(x = estimate)) + geom_histogram()

model_perf_nested <- gap_models %>% mutate(fit = map(model, ~glance(.x)))
model_perf <- model_perf_nested %>% unnest(fit)

model_perf %>% ggplot(aes(x = r.squared)) + geom_histogram()
best_fit <- model_perf %>% ungroup() %>% top_n(n = 4, wt = r.squared)
worst_fit <- model_perf %>% ungroup() %>% top_n(n = 4, wt = -r.squared)

best_augmented <- best_fit %>% mutate(augmented = map(model,augment)) %>% unnest(augmented)
worst_augmented <- worst_fit %>% mutate(augmented = map(model,augment)) %>% unnest(augmented)

best_augmented %>% 
  ggplot(aes(x = year)) +
  geom_point(aes(y = .fitted)) + 
  geom_line(aes(y = lifeExp), color = "red") +
  facet_wrap(~country, scales = "free_y")

worst_augmented %>% 
  ggplot(aes(x = year)) +
  geom_point(aes(y = .fitted)) + 
  geom_line(aes(y = lifeExp), color = "red") +
  facet_wrap(~country, scales = "free_y")

# Multiple regression model
unnest(gap_nested,data)
gap_fullmodel <- gap_nested %>% mutate(model = map(data, ~lm(formula =lifeExp~year+pop+gdpPercap, data = .x)))

fullmodel_perf <- gap_fullmodel %>% mutate(fit = map(model, ~glance(.x))) %>% unnest(fit)

fullmodel_perf %>% filter(country %in% worst_fit$country) %>% select(country, adj.r.squared)

set.seed(42)
gap_split <- initial_split(gapminder, prop = 0.75)
training_data <- training(gap_split)
testing_data <- testing(gap_split)

dim(training_data)
dim(testing_data)

# Prepare the dataframe containing the cross validation partitions
cv_split <- vfold_cv(training_data, v = 5)

cv_data <- cv_split %>% 
  mutate(
    # Extract the train dataframe for each split
    train = map(splits, ~training(.x)), 
    # Extract the validate dataframe for each split
    validate = map(splits, ~testing(.x))
  )

# Use head() to preview cv_data
head(cv_data)

cv_models_lm <- cv_data %>% 
  mutate(model = map(train, ~lm(formula = lifeExp~., data = .x)))

cv_prep_lm <- cv_models_lm %>% 
  mutate(
    # Extract the recorded life expectancy for the records in the validate dataframes
    validate_actual = map(validate, ~.x$lifeExp),
    # Predict life expectancy for each validate set using its corresponding model
    validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y))
  )

cv_eval_lm <- cv_prep_lm %>% 
  mutate(validate_mae = map2_dbl(validate_actual, validate_predicted, ~mae(actual = .x, predicted = .y)))

# Print the validate_mae column
cv_eval_lm$validate_mae

mean(cv_eval_lm$validate_mae)

gapminder$lifeExp
cv_models_rf <- cv_data %>% mutate(model = map(train, ~ranger(formula = lifeExp~., data = .x, num.trees = 100, seed = 42)))

cv_prep_rf <- cv_models_rf %>% 
  mutate(validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y)$predictions),
         validate_actual = map(validate, ~.x$lifeExp))


cv_eval_rf <- cv_prep_rf %>% 
  mutate(validate_mae = map2_dbl(validate_actual, validate_predicted, ~mae(actual = .x, predicted = .y)))

cv_eval_rf$validate_mae
mean(cv_eval_rf$validate_mae)
