# Based on Python blogpost on text classificaton https://medium.com/@bedigunjit/simple-guide-to-text-classification-nlp-using-svm-and-naive-bayes-with-python-421db3a72d34
library(recipe)

corpus <- readr::read_csv("svm_textclassification/corpus.csv") 

corpus <- corpus %>% mutate(Id = row_number(), label = factor(label))

# Seems that we can go straight to the recipe

text_recipe <- recipe(label ~ ., data = corpus) %>% update_role(Id, new_role = "ID") %>% 
                      step_tokenize(text) %>% 
                      step_stopwords(text, language = "es") %>%
                      step_tokenfilter(text) %>% 
                      step_tf(text)


juice(prep(text_recipe)) %>% View()