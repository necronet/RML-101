# Modeling process Sample techniques
set.seed(11052019)

# libraries dependencies 
library(ggplot2)
library(rsample)
library(dplyr)
library(gridExtra)
library(caret)

# Dataset dependencies
library(AmesHousing)

ames <- AmesHousing::make_ames()

# Config variables
sample_rate <- .7

# Splitting data with simple random sample
index_1 <- sample(1:nrow(ames),round(nrow(ames) * sample_rate ))
train_1 <- ames[index_1, ]
test_1 <- ames[-index_1, ]

# Splittin data with random sample but with rsample package
split_1 <- initial_split(ames, prop = sample_rate)
train_2 <- training(split_1)
test_2 <- testing(split_1)

plot_distributions <- function(d1, d2, d3, title) {
  ggplot() + 
    ggtitle(title) +
    geom_density(data=data.frame(d1), aes(x=Sale_Price), fill="#154591", alpha = 0.6, color='#154591') + 
    geom_density(data=data.frame(d2), aes(x=Sale_Price), fill="#f4ee42", alpha = 0.4, color='#f4ee42') +
    geom_density(data=data.frame(d3), aes(x=Sale_Price), fill="#39A114", alpha = 0.3, color="#39A114")
}

plot_distributions(ames, train_1,test_1, 'ame, test, and train distribution')


# Stratified sample
table(churn$Attrition) %>% prop.table()

# Using rsample package
split_strat <- initial_split(churn, prop=sample_rate, strate='Attrition')
train_strata <- training(split_strat)
test_strata <- testing(split_strat)


table(train_strata$Attrition) %>% prop.table()
table(test_strata$Attrition) %>% prop.table()

plot_strata_sample <- function(global, training, testing) {
  plot_strata_global <- ggplot() + geom_bar(data=global, aes(Attrition), fill="#154591") + ggtitle('Global')
  plot_strata_training <- ggplot() + geom_bar(data=training, aes(Attrition), fill="#f4ee42") + ggtitle('Training')
  plot_strata_test <- ggplot() + geom_bar(data=testing, aes(Attrition), fill="#39A114") + ggtitle('Testing')  
  grid.arrange(plot_strata_global, plot_strata_training, plot_strata_test, nrow = 3)
}

plot_strata_sample(churn, train_strata, test_strata)


# Putting all togethe
split <- initial_split(ames, prop = sample_rate, strata="Sale_Price")
ames_training <- training(split)
ames_testing <- testing(split)

cv <- trainControl( method = 'repeatedcv', number = 10, repeats = 5, verboseIter = TRUE)

# Hyper parameter for grid search
hyper_grid <- expand.grid(k = seq(2, 25, by=1))
knn_fit <- train(Sale_Price ~., data = ames_training, method='knn', 
                 trControl=cv, tuneGrid= hyper_grid,
                 metric='RMSE')

lm_fit <- train(Sale_Price ~., data = ames_training, method='lm', trControl=cv, metric='MSE')




