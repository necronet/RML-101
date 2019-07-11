library(earth)
library(AmesHousing)
library(rsample)
library(caret)


set.seed(11052019)

ames <- AmesHousing::make_ames()

sample_rate <- 0.7

split <- rsample::initial_split(ames, prop=sample_rate)
ames_training <- rsample::training(split)
ames_testing <- rsample::testing(split)

mars <- earth(Sale_Price ~., data = ames_training)

summary(mars) %>% .$coefficients %>% head(10)

plot(mars, which = 1)


mars2 <- earth(
  Sale_Price ~ .,  
  data = ames_training,
  degree = 2
)

summary(mars2) %>% .$coefficients %>% head(10)
plot(mars2, which = 1)

# Pruning process

hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = seq(2, 100, length.out = 10) %>% floor()
)

cv_mars <- train( x = subset(ames_training, select=-Sale_Price),
                  y = ames_training$Sale_Price,
                  method = 'earth',
                  metric = 'RMSE',
                  trControl = trainControl(method='cv', number = 10, verboseIter = TRUE),
                  tuneGrid = hyper_grid
                  )

cv_mars$bestTune
summary(cv_mars$finalModel)

ggplot(cv_mars)


p1 <- vip(cv_mars, num_features = 40, bar = FALSE, value = "gcv") + ggtitle("GCV")
p2 <- vip(cv_mars, num_features = 40, bar = FALSE, value = "rss") + ggtitle("RSS")

gridExtra::grid.arrange(p1, p2, ncol = 2)


cv_mars$finalModel %>%
  coef() %>%  
  broom::tidy() %>% 
  filter(stringr::str_detect(names, "\\*"))



