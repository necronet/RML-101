library(readr)
library(dplyr)
library(magrittr)
library(tidytext)
library(stringr)
# Constant section

FILE_ADDRESS_1 <- "direcciones/data/clientes_rutas.csv"
TARGET_ADDRESS_FILE = "direcciones/data/address_filtered.csv"

# TODO: missing documentation
preprocess_original_file <- function(source_file = FILE_ADDRESS_1, target_file = TARGET_ADDRESS_FILE) {
  #Read oroginal file
  readr::read_csv("direcciones/data/clientes_rutas.csv") %>% 
    #treatment to remove commas and dots
    mutate_all(str_replace_all, pattern="[\\.,]", replacement = " ") %>%
    #remove white space on 1 c to 1c
    mutate_all(str_replace_all, pattern="(\\d)(\\s+)[cC]\\s+", replacement = "\\1") %>%
    mutate_all(str_replace_all, pattern="\\d/\\d[cC]\\s+", replacement = "NC ") %>%
    # replace any \dC (like 1C 2c 3c) with a general NC
    mutate_all(str_replace_all, pattern="\\d[cC]\\s+", replacement = "NC ") %>%
    #Select only features that are interesting
    select(Department, Municipality, Address1, Address2, Neighborhood) %>% 
    # filter out NA values for department or municipality
    filter(!is.na(Department) & !is.na(Municipality)) %>% 
    # all data must be upper case for homogenity no special meaning between upper and lowercase
    mutate_all(str_to_upper) %T>%
    write_csv(target_file) 
}

# TODO: missing documentation
get_address <- function(addresses, token = "words", ...) {
  # Having the address1 and address2 together for unigram count
  addresses %>% mutate(Address = paste(Address1, str_replace_na(Address2, "")), doc_id = row_number()) %>% 
    # Select only the newly created Address column and department
    select(Department, Address, doc_id) %>% 
    # Getting all the words for each Address
    unnest_tokens(word, Address, token = token, ...)  
}

# TODO: MISSING roxygen
deparment_word_count <- function (address_words) {
  address_words %>% count(Department, word, sort = TRUE)
}

# TODO: missing documentation
get_freq_by_rank <- function (addresses, top_n = 5) {
  total_word_count_department <- addresses %>% deparment_word_count %>% group_by(Department) %>% summarise(total = sum(n))
  total_word_per_department <- addresses %>% deparment_word_count
  freq_term <- left_join(total_word_per_department, total_word_count_department) %>% group_by(Department) %>%
    top_n(top_n, n) %>% 
    mutate(rank = row_number(), freq_term = n/total)
}

## Tokenization of Address into n-grams

address_ngram_count <- function(addresses = preprocess_original_file(), ngram = 2) {
  stop_words_es <- stopwords::stopwords(language = "es")
  address_data_ngrams <- addresses %>% get_address("ngrams", n = ngram) 
  #address_data_ngrams %>% count(word, sort = TRUE)
  address_data_ngrams %>% separate(word, sprintf("word%s",seq(1: ngram))) %>% 
    filter_at(vars(starts_with("word")), all_vars(!. %in% stop_words_es )) %>%
    group_by_at(vars(starts_with("word"))) %>% count(sort=TRUE)
}

address_ngram <- function(addresses = preprocess_original_file(), ngram = 2) {
  stop_words_es <- stopwords::stopwords(language = "es")
  address_data_ngrams <- addresses %>% get_address("ngrams", n = ngram) 
  #address_data_ngrams %>% count(word, sort = TRUE)
  address_data_ngrams %>% separate(word, sprintf("word%s",seq(1: ngram))) %>% 
    filter_at(vars(starts_with("word")), all_vars(!. %in% stop_words_es )) 
}


