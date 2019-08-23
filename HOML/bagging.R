library(dplyr)
library(ggplot2)
library(doParallel)
library(foreach)

library(caret)
library(rpart)
library(ipred)
library(rsample)

library(AmesHousing)

ames <- AmesHousing::make_ames()

sample_rate <- 0.75

split_ames <- initial_split(data = ames, prop = sample_rate)
ames_training <- training(split_ames)
ames_testing <- testing(split_ames)



# Seed parameter
set.seed(11052019)
errors <- vector()
for ( i in seq(from = 20, to = 100) ) {
  print(paste('Bagging with',i,'trees'))
  ames_bag_i <- bagging( formula = Sale_Price ~ ., data=ames_training, nbagg = i, coob=T, control = rpart.control(minsplit = 2, cp = 0))
  errors <- c(errors, ames_bag_i$err) 
  print(ames_bag_i$err)
}
error_df <- as.data.frame(errors)
error_df$n_trees <- seq(20, to = 20+nrow(error_df)-1)

ggplot(data = error_df, aes(x = n_trees, y = errors)) + geom_line()

