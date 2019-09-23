library(dplyr)
library(ggplot2)
library(rsample)
library(caret)
library(kernlab)
library(pdp)
library(vip)

df <- attrition %>% mutate_if(is.ordered, factor, ordered = FALSE)

set.seed(11052019)
split_rate <- 0.7
churn_split <- initial_split(df, prop = split_rate, strata = "Attrition")
churn_train <- training(churn_split)
churn_test  <- testing(churn_split)

churn_svm <- train(
  Attrition ~ ., 
  data = churn_train,
  method = "svmRadial",               
  preProcess = c("center", "scale"),  
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10
)

ggplot(churn_svm) + theme_light()

churn_svm$bestTune
churn_svm$results

class.weights = c("No" = 1, "Yes" = 10)


ctrl <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE,                 
  summaryFunction = twoClassSummary  # also needed for AUC/ROC
)

# Tune an SVM
churn_svm_auc <- train(
  Attrition ~ ., 
  data = churn_train,
  method = "svmRadial",               
  preProcess = c("center", "scale"),  
  metric = "ROC",  # area under ROC curve (AUC)       
  trControl = ctrl,
  tuneLength = 10
)


churn_svm_auc$results
churn_svm_auc$bestTune
ggplot(churn_svm_auc) + theme_light()


confusionMatrix(churn_svm_auc)

prob_yes <- function(object, newdata) {
  predict(object, newdata = newdata, type = "prob")[, "Yes"]
}

vip(churn_svm_auc, method = "permute", nsim = 5, train = churn_train, 
    target = "Attrition", metric = "auc", reference_class = "Yes", 
    pred_wrapper = prob_yes)



