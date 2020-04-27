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


(congress_dtm <- congress_tokens %>%
    # get count of each token in each document
    count(ID, word) %>%
    # create a document-term matrix with all features and tf weighting
    cast_dtm(document = ID, term = word, value = n))

str(congress_dtm)

congress_dtm$dimnames




