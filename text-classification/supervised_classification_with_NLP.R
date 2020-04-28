# Based on university of illinois post https://cfss.uchicago.edu/notes/supervised-text-classification/
library(tidyverse)
library(tidytext)
library(stringr)
library(caret)
library(tm)
theme_set(theme_minimal())

# Loading data
data(USCongress, package = "rcfss")

# topic labels
major_topics <- tibble(
  major = c(1:10, 12:21, 99),
  label = c("Macroeconomics", "Civil rights, minority issues, civil liberties",
            "Health", "Agriculture", "Labor and employment", "Education", "Environment",
            "Energy", "Immigration", "Transportation", "Law, crime, family issues",
            "Social welfare", "Community development and housing issues",
            "Banking, finance, and domestic commerce", "Defense",
            "Space, technology, and communications", "Foreign trade",
            "International affairs and foreign aid", "Government operations",
            "Public lands and water management", "Other, miscellaneous")
)

(congress <- as_tibble(USCongress) %>% left_join(major_topics))


(congress_tokens <- congress %>%
    unnest_tokens(output = word, input = text) %>%
    # remove numbers
    filter(!str_detect(word, "^[0-9]*$")) %>%
    # remove stop words
    anti_join(stop_words) %>%
    # stem the words
    mutate(word = SnowballC::wordStem(word)))


(congress_dtm <- congress_tokens %>%    # get count of each token in each document
    count(ID, word) %>%
    # create a document-term matrix with all features and tf weighting
    cast_dtm(document = ID, term = word, value = n))

congress_tokens %>%
  # get count of each token in each document
  count(ID, word) %>%
  # create a document-term matrix with all features and tf-idf weighting
  cast_dtm(document = ID, term = word, value = n,
           weighting = tm::weightTfIdf)

removeSparseTerms(congress_dtm, sparse = 0.99) %>% as.matrix()
removeSparseTerms(congress_dtm, sparse = 0.95) %>% as.matrix()

str(congress_dtm)

congress_dtm$dimnames$Docs
congress_dtm$dimnames$Terms

str(congress_dtm[1,])
str(congress_dtm[,1])

(congress_dtm <- removeSparseTerms(congress_dtm, sparse = .99))


congress_rf <- train(x = as.matrix(congress_dtm),
                     y = factor(congress$major),
                     method = "ranger",
                     num.trees = 200,
                     trControl = trainControl(method = "oob", verboseIter = TRUE))


(congress_slice <- slice(congress, as.numeric(congress_dtm$dimnames$Docs)))
congress_rf_10 <- train(x = as.matrix(congress_dtm),
                        y = factor(congress_slice$major),
                        method = "ranger",
                        num.trees = 10,
                        importance = "impurity",
                        trControl = trainControl(method = "oob", verboseIter = TRUE))

congress_rf_10$finalModel %>%
  # extract variable importance metrics
  ranger::importance() %>%
  # convert to a data frame
  enframe(name = "variable", value = "varimp") %>%
  top_n(n = 20, wt = varimp) %>%
  # plot the metrics
  ggplot(aes(x = fct_reorder(variable, varimp), y = varimp)) +
  geom_col() +
  coord_flip() +
  labs(x = "Token",
       y = "Variable importance (higher is more important)")

congress_rf_200 <- train(x = as.matrix(congress_dtm),
                         y = factor(congress_slice$major),
                         method = "ranger",
                         num.trees = 200,
                         importance = "impurity",
                         trControl = trainControl(method = "oob"))


congress_rf_200$finalModel %>%
  # extract variable importance metrics
  ranger::importance() %>%
  # convert to a data frame
  enframe(name = "variable", value = "varimp") %>%
  top_n(n = 20, wt = varimp) %>%
  # plot the metrics
  ggplot(aes(x = fct_reorder(variable, varimp), y = varimp)) +
  geom_col() +
  coord_flip() +
  labs(x = "Token",
       y = "Variable importance (higher is more important)")