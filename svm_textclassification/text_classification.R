# Based on Python blogpost on text classificaton https://medium.com/@bedigunjit/simple-guide-to-text-classification-nlp-using-svm-and-naive-bayes-with-python-421db3a72d34
library(readr)
library(recipes)
library(discrim)
library(yardstick)
library(parsnip)
library(tune)
library(parallel)
library(textrecipes)
library(workflows)
library(rsample)
library(dplyr)
library(ggplot2)

detectCores()
doParallel::registerDoParallel()

loading_data <- function(path) {
  readr::read_csv(path, locale = locale(encoding = "latin1"))
}

corpus <- loading_data("svm_textclassification/corpus.csv") %>% 
          mutate(Id = row_number(), label = factor(label)) %>% na.omit()

text_split <- initial_split(corpus)
training_set <- training(text_split)
test_set <- testing(text_split)

# Seems that we can go straight to the recipe
createRecipe <- function(data) {
  recipe(label ~ ., data = data) %>% update_role(Id, new_role = "ID") %>% 
    step_tokenize(text, engine = "spacyr") %>% 
    step_stopwords(text) %>%
    step_lemma(text) %>%
    step_tokenfilter(text, max_tokens = 100) %>%
    step_tfidf(text)
}
text_recipe <- createRecipe(training_set)

text_model_NB_spec <- naive_Bayes() %>% set_engine("naivebayes") %>% set_mode("classification")
text_model_svm_spec <- svm_poly("classification") %>% set_engine("kernlab")

text_model_NB_wf <- workflows::workflow() %>% add_recipe(text_recipe) %>% add_model(text_model_NB_spec)
text_model_svm_wf <- workflows::workflow() %>% add_recipe(text_recipe) %>% add_model(text_model_svm_spec)

fit_NB_model <- fit(text_model_NB_wf, training_set)
fit_svm_model <- fit(text_model_svm_wf, training_set)

predictions_NB <- predict(fit_NB_model, test_set)
predictions_SVM <- predict(fit_svm_model, test_set)

bind_cols(test_set,predictions_NB) %>% conf_mat(label, .pred_class) 
bind_cols(test_set,predictions_NB) %>% accuracy(truth = label, estimate = .pred_class)

bind_cols(test_set,predictions_SVM) %>% conf_mat(label, .pred_class) 
bind_cols(test_set,predictions_SVM) %>% accuracy(truth = label, estimate = .pred_class)

prediction_NB_prob <- predict(fit_NB_model, test_set, type = "prob")
prediction_SVM_prob <- predict(fit_svm_model, test_set, type = "prob")

roc_NB <- bind_cols(test_set,prediction_NB_prob) %>% roc_curve(label, .pred___label__1) %>% mutate(Model="Naive Bayes") 
roc_SVM <-  bind_cols(test_set,prediction_SVM_prob) %>% roc_curve(label, .pred___label__1) %>% mutate(Model="SVM")

bind_rows(roc_NB,roc_SVM) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, color=Model)) + 
  geom_path() + geom_abline(lty = 3) + 
  coord_equal() + theme_bw() + 
  ggtitle("ROC SVM vs Naive bayes","Receiver Operator curve comparing support vector machine \nand naive bayes without hyperparameter tunning") 




