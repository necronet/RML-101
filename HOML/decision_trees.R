# Example HOML of Decision trees
set.seed(11052019)
library(ggplot2)
library(caret)
library(rsample)
library(rpart)
library(rpart.plot)
library(vip)
library(AmesHousing)

ames <- AmesHousing::make_ames()
split_1 <- initial_split(ames, prop = 0.7)
ames_training <- training(split_1)
ames_testing <- testing(split_1)

ames_dt1 <- rpart(
  formula = Sale_Price ~ .,
  data    = ames_training,
  method  = "anova"
)


rpart.plot(ames_dt1)
plotcp(ames_dt1)


ames_dt2 <- rpart(
  formula = Sale_Price ~ .,
  data    = ames_training,
  method  = "anova", 
  control = list(cp = 0, xval = 10)
)

plotcp(ames_dt2)
abline(v = 12, lty = "dashed")


ames_dt3 <- train(
  Sale_Price ~ .,
  data = ames_training,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 20
)

ggplot(ames_dt3)

vip(ames_dt3, num_features = 40, bar = FALSE)