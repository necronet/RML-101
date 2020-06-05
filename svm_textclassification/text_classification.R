# Based on Python blogpost on text classificaton https://medium.com/@bedigunjit/simple-guide-to-text-classification-nlp-using-svm-and-naive-bayes-with-python-421db3a72d34
library(recipes)
library(discrim)
library(yardstick)
library(parsnip)
library(tune)
library(parallel)
library(textrecipes)
library(workflows)
library(rsample)
library(ggplot2)

detectCores()
doParallel::registerDoParallel()
corpus <- readr::read_csv("svm_textclassification/corpus.csv", locale = readr::locale(encoding = "latin1"))

corpus <- corpus %>% mutate(Id = row_number(), label = factor(label)) %>% na.omit()

text_split <- initial_split(corpus)
training_set <- training(text_split)
test_set <- testing(text_split)

# Seems that we can go straight to the recipe

text_recipe <- recipe(label ~ ., data = training_set) %>% update_role(Id, new_role = "ID") %>% 
  step_tokenize(text, engine = "spacyr") %>% 
  step_stopwords(text) %>%
  step_lemma(text) %>%
  step_tokenfilter(text, max_tokens = 100) %>%
  step_tfidf(text)

text_model_spec <- naive_Bayes() %>% set_engine("naivebayes") %>% set_mode("classification")
text_model_svm_spec <- svm_poly("classification") %>% set_engine("kernlab")

text_model_wf <- workflows::workflow() %>% add_recipe(text_recipe) %>% add_model(text_model_spec)
text_model_svm_wf <- workflows::workflow() %>% add_recipe(text_recipe) %>% add_model(text_model_svm_spec)

ptm <- proc.time()
fit_model <- fit(text_model_wf, training_set)
fit_svm_model <- fit(text_model_svm_wf, training_set)

print(paste0("spend time: ", (proc.time() - ptm)))


predictions <- predict(fit_model, test_set)
bind_cols(test_set,predictions) %>% conf_mat(label, .pred_class) 
bind_cols(test_set,predictions) %>% accuracy(truth = label, estimate = .pred_class)


svm_predictions <- predict(fit_svm_model, test_set, type = "prob")
bind_cols(test_set,svm_predictions) %>% conf_mat(label, .pred_class) 
bind_cols(test_set,svm_predictions) %>% accuracy(truth = label, estimate = .pred_class)

bind_cols(test_set,svm_predictions) %>% 
          roc_curve(label, .pred___label__1) %>%
          ggplot(aes(x = 1 - specificity, y = sensitivity)) + 
          geom_path() + geom_abline(lty = 3) + 
          coord_equal() + theme_bw()
          




