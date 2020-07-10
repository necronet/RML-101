library(readr)
library(dplyr)
library(skimr)
library(purrr)
library(forcats)
library(ggplot2)
library(rsample)
library(glmnet)
library(stringr)
library(tidyr)
library(DMwR)
library(corrplot)
library(GGally)


pokemon_data <- readr::read_csv("pokemon/pokemon.csv")

map_fn <- function(x) {
  length(str_split(x, ",", simplify = TRUE))
}

get_maximum_abilities <- function(pokemon_df) {
  pokemon_df %>% pull(abilities) %>% map_int(map_fn) %>% max()  
}

clean_ability <- function(ability) {
  gsub("\\[|\\]|\\'","", ability)  
}

max_abilities <- pokemon_data %>% get_maximum_abilities()

skimr::skim(pokemon_df)

pokemon_df <- pokemon_data %>% na.omit() %>%
    mutate(abilities = clean_ability(abilities)) %>% 
    separate('abilities', paste0("ability",1:max_abilities), sep=',') %>%
    mutate(is_legendary = factor(is_legendary), 
           type1 = factor(type1), generation = factor(generation)) %>% 
    mutate_at(vars(starts_with("ability")), factor) %>%
             select(-starts_with("against")) 

pokemon_data %>% na.omit() %>% 
    select_if(~ is_double(.x) | is_integer(.x)) %>% 
    select(-starts_with("against")) %>% cor %>% corrplot(method = "number")

pokemon_data %>% na.omit() %>% 
  select_if(~ is_double(.x) | is_integer(.x)) %>%
  select(-starts_with("against")) %>% 
  mutate(is_legendary = factor(is_legendary)) %>%
  ggpairs(aes(color = is_legendary, alpha = 0.8))

pokemon_df %>% count(ability1, sort=T) %>% 
                  ggplot(aes(fct_reorder(ability1, n), n)) + 
                  geom_col() + coord_flip()

pokemon_df %>% count(type1, is_legendary, sort = TRUE) %>% 
                 ggplot(aes(forcats::fct_reorder(type1, n), n, fill = is_legendary)) + 
                 geom_col() + coord_flip() 


pokemon_df %>% ggplot(aes(x = height_m, y = weight_kg, label = name)) + 
               geom_point(alpha = 0.6, aes(color = generation)) + 
               geom_text(data = pokemon_df,
                          aes(label = ifelse(weight_kg > 500 | height_m > 7, as.character(name), '')),
                          hjust=0, vjust=0) + 
              geom_smooth(formula = y ~ x)

pokemon_df %>% group_by(type1) %>% 
            summarise(n = n(), weight_avg = mean(weight_kg), 
                               height_avg = mean(height_m)) %>%
  ggplot(aes(x = height_avg, y = weight_avg, size = n)) + 
  geom_point(alpha = 0.6) +
  guides(color=guide_legend(), size = guide_legend())


# Let's classify legendary from non legendary for binary classification
pokemon_design <- pokemon_df %>% select(-starts_with("ability"))

pokemon_split <- initial_split(pokemon_design,prop = 0.6)
# See what I did there? hahaha
training_pokemon <- training(pokemon_split)
testing_pokemon <- testing(pokemon_split)

training_pokemon %>% count(is_legendary)

smoted_training_pokemon <- SMOTE(is_legendary ~ ., as.data.frame(
                                  select_if(training_pokemon, ~is_numeric(.x))),
                                  perc.over = 500, k = 2) %>% na.omit() 

smoted_training_pokemon %>% count(is_legendary)
 
smoted_training_pokemon %>% ggplot(aes(x = height_m, y = weight_kg)) + 
  geom_point(alpha = 0.6, aes(color = is_legendary)) + 
  geom_smooth(formula = y ~ x)

x <- data.matrix(smoted_training_pokemon %>% select(-is_legendary))
y <- smoted_training_pokemon$is_legendary
# Simply using glmp for classification no recipe or no workflow or tunnning just yet
fit = cv.glmnet(x, y, family = "binomial")

plot(fit)
plot(fit$glmnet.fit)

intercept <- coef(fit)[1,]
fit_height_m <- coef(fit)[2,]
fit_weight_m <- coef(fit)[3,]


pred = predict(fit, data.matrix(testing_pokemon %>%  select_if(is_numeric) %>% select(-is_legendary)),  
        type = "class", s = 'lambda.min' )

bind_cols(.pred = pred, testing_pokemon) %>% 
  mutate(correct = .pred == is_legendary) %>%
  ggplot(aes(x = height_m, y = weight_kg, color = .pred, shape = correct)) + 
  scale_shape_manual(values=c(4, 19)) +
  geom_point(alpha = 0.8, size = 3)

# Confusion matrix
bind_cols(.pred = pred, testing_pokemon) %>%
  mutate(correct = case_when(.pred == is_legendary ~ 1, T ~ 0)) %>% 
  select(is_legendary, .pred) %>% table() 





