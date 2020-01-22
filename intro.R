# Exploring the tidyverse with ML
library(gapminder)
library(purrr)
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)

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
best_fit <- model_perf %>% top_n(n = 4, wt = r.squared)
worst_fit <- model_perf %>%  top_n(n = 4, wt = -r.squared)


best_augmented <- best_fit %>% mutate(augmented = map(model,augment)) %>% unnest(model)










