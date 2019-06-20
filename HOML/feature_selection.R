library(recipes)
library(ggplot2)
library(rsample)
library(forecast)

# Dataset dependencies
library(AmesHousing)

sample_rate <- 0.75
lambda <- 0.3
# Use for Box-Cox transformation


ames <- AmesHousing::make_ames()

split_1 <- initial_split(ames, prop = sample_rate)
training_ames <- training(split_1)
testing_ames <- testing(split_1)

# Plotting skewness of Sale_Price and  how to fix it with a log transformation
ggplot(data.frame(price=ames$Sale_Price)) + geom_histogram(aes(x=price))
ggplot(data.frame(log_price=log(ames$Sale_Price)),aes(x=log_price)) + geom_histogram(bins=45) 
ggplot(data.frame(box_cox_price=forecast::BoxCox(ames$Sale_Price, lambda)),aes(x=box_cox_price)) + geom_histogram(bins=45) 

ames_recipes <- recipe(Sale_Price ~., data=training_ames) %>% step_log(all_outcomes())


ames_whole <- data.frame(sample_n(ames, 500))
# random sample of elements that will be imputed as a comparison between different
# method mean, knn, tree bag

sample_indexes <- sample(1:nrow(ames_whole), 50)
ames_imputation_sample <- ames_whole[sample_indexes,]
ames_whole[sample_indexes,]$Gr_Liv_Area <- NA

ames_imputed_mean <- recipe(Sale_Price ~., data=ames_whole) %>% 
  step_medianimpute(Gr_Liv_Area)  %>% 
  prep %>% 
  bake(new_data=ames_whole)

ames_imputed_knn <- recipe(Sale_Price ~., data=ames_whole) %>% 
  step_knnimpute(Gr_Liv_Area)  %>% 
  prep %>% 
  bake(new_data=ames_whole)

ames_imputed_bag <- recipe(Sale_Price ~., data=ames_whole) %>% 
  step_bagimpute(Gr_Liv_Area)  %>% 
  prep %>% 
  bake(new_data=ames_whole)

ames_without_vals <- ames_whole[-sample_indexes,]

ggplot(NULL, aes(x=Gr_Liv_Area, y=Sale_Price)) + 
  geom_point(data=ames_without_vals)  +
  geom_point(data=ames_imputation_sample, color='red') +
#  geom_point(data=ames_imputed_mean[sample_indexes, ], color='blue') +
#  geom_point(data=ames_imputed_knn[sample_indexes, ], color='red') + 
  geom_point(data=ames_imputed_bag[sample_indexes, ], color='yellow')




