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

pokemon_data <- readr::read_csv("pokemon/pokemon.csv")



map_fn <- function(x) {
  length(str_split(x, ",", simplify = TRUE))
}

get_maximum_abilities <- function(pokemon_df) {
  pokemon_df %>% pull(abilities) %>% map_int(map_fn) %>% max()  
}

max_abilities <- pokemon_df %>% get_maximum_abilities()

pokemon_df %>% separate('abilities', paste0("abilities",1:max_abilities), sep=',')

pokemon_df <- pokemon_data %>% na.omit() %>%
    mutate(is_legendary = factor(is_legendary), type1 = factor(type1), generation = factor(generation)) %>% 
             select(-starts_with("against"))

skimr::skim(pokemon_df)

pokemon_df %>% count(type_merged)

pokemon_df %>% count(type1, is_legendary, sort = TRUE) %>% 
                 ggplot(aes(forcats::fct_reorder(type1, n), n, fill = is_legendary)) + 
                 geom_col() + coord_flip() 


pokemon_df %>% ggplot(aes(x = height_m, y = weight_kg, label = name)) + 
               geom_point(alpha = 0.6, aes(color = generation)) + 
               geom_text(data = pokemon_df,
                          aes(label = ifelse(weight_kg > 500 | height_m > 7, as.character(name), '')),
                          hjust=0, vjust=0) + 
              geom_smooth(formula = y ~ x)

pokemon_df %>% na.omit() %>% group_by(type1) %>% 
            summarise(n = n(), weight_avg = mean(weight_kg), 
                               height_avg = mean(height_m)) %>%
  ggplot(aes(x = height_avg, y = weight_avg, size = n)) + 
  geom_point(alpha = 0.6) +
  guides(color=guide_legend(), size = guide_legend())


# Let's classify legendary from non legendary for binary classification
pokemon_design <- pokemon_df %>% select(name, height_m, weight_kg, is_legendary, type1)

pokemon_split <- initial_split(pokemon_design, strata = is.legendary)
# See what I did there? hahaha
training_pokemon <- training(pokemon_split)
testing_pokemon <- testing(pokemon_split)

pokemon_design %>% count(is_legendary)

x <- data.matrix(training_pokemon %>% select(-name, is_legendary))
y <- training_pokemon$is_legendary
# Simply using glmp for classification no recipe or no workflow or tunnning just yet
fit = cv.glmnet(x, y, family = "binomial")




