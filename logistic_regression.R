library(AmesHousing)
library(rsample)
library(dplyr)  
library(caret) 
library(ROCR)

set.seed(11052019)

# Config variables
sample_rate <- .7

churn <- rsample::attrition %>% mutate_if(is.ordered, factor, ordered = FALSE)

split <- initial_split(churn, prop = sample_rate)
churn_train <- training(split)
churn_test <- testing(split)

lr_model1 <- glm(Attrition ~ MonthlyIncome, family = "binomial", data = churn_train)
lr_model2 <- glm(Attrition ~ OverTime, family = "binomial", data = churn_train)

# Multivariate logistic regression similar to linear regression example

lr_model3 <- glm(Attrition ~ MonthlyIncome + OverTime, family = "binomial", data = churn_train)

# Assesing accuracy

# Use of caret to train multiple models with cross-validation technique

lr_model1 <- train(Attrition ~ MonthlyIncome, data = churn_train, 
                   method = 'glm', family = 'binomial', 
                   trControl = trainControl(method = 'cv', number = 10, verboseIter = TRUE))

lr_model2 <- train(Attrition ~ MonthlyIncome + OverTime, family = 'binomial', method = 'glm', data = churn_train,
                   trControl = trainControl(method = 'cv', number = 10, verboseIter = TRUE))

lr_model3 <- train(Attrition ~ ., family = 'binomial', method = 'glm', data = churn_train, 
                   trControl = trainControl(method = 'cv', number = 10, verboseIter = TRUE))

summary(resamples(list(model1 = lr_model1, model2 = lr_model2, model3 = lr_model3)))$statistics$Accuracy

predicted_class <- predict(lr_model3, churn_train)

confusionMatrix( data = relevel(predicted_class, ref = 'Yes'), reference = relevel(churn_train$Attrition, ref ='Yes'))

# This will work but put the no first relevel take care of this by reordering the factor 
# confusionMatrix( data = predicted_class, reference = churn_train$Attrition)


m1_prob <- predict(lr_model1, churn_train, type = "prob")$Yes
m2_prob <- predict(lr_model2, churn_train, type = "prob")$Yes
m3_prob <- predict(lr_model3, churn_train, type = "prob")$Yes

perf1 <- prediction(m1_prob, churn_train$Attrition) %>% performance(measure = 'tpr', x.measure = 'fpr')
perf2 <- prediction(m2_prob, churn_train$Attrition) %>% performance(measure = 'tpr', x.measure = 'fpr')
perf3 <- prediction(m3_prob, churn_train$Attrition) %>% performance(measure = 'tpr', x.measure = 'fpr')

plot(perf1, col = "black", lty = 2)
plot(perf2, add = TRUE, col = "blue")
plot(perf3, add = TRUE, col = "red")
legend(0.8, 0.2, legend = c("lr_model1", "lr_model2", "lr_model3"),
       col = c("black", "blue", "red"), lty = 2:1, cex = 0.6)

# Principal component scoring

cv_model_pls <- train(
  Attrition ~ ., 
  data = churn_train, 
  method = "pls",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 16
)

# Model with lowest RMSE
cv_model_pls$bestTune

# Plot cross-validated RMSE
ggplot(cv_model_pls)

