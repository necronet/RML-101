# N-grams and correlations
library(tidyr)
library(dplyr)
library(tidytext)
library(janeaustenr)

austen_bigrams <- austen_books() %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams %>% count(bigram, sort = TRUE)


bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


bigram_counts <- bigrams_filtered %>% count(word1, word2, sort = TRUE)
bigrams_united <- bigrams_filtered %>% unite(bigram, word1, word2, sep = " ")


austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)


bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)


bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf)) %>% group_by(book) %>% 
  do(head(., n = 10)) %>% ungroup()

bigram_tf_idf %>% mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
    ggplot(aes(bigram, tf_idf, fill=book)) + 
    geom_col(show.legend = FALSE) + 
    labs(x = NULL, y = "tf-idf") +
    coord_flip() + xlab(NULL) + 
    facet_wrap(~book, ncol = 2, scales = "free")



