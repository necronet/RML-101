library(tm)
library(dplyr)
library(tidytext)
library(ggplot2)
library(quanteda)
library(tidyr)
library(Matrix)
library(janeaustenr)

data("AssociatedPress", package = "topicmodels")


terms <- Terms(AssociatedPress)

#Transforming into a tidy object
ap_td <- tidy(AssociatedPress)


ap_sentiments <- ap_td %>% inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments %>% count(sentiment, term, wt = count) %>% 
              filter(n >= 200) %>% 
              mutate(n = ifelse(sentiment=="negative", -n, n)) %>%
              mutate(term = reorder(term, n)) %>%
              ggplot(aes(term, n, fill = sentiment)) + geom_bar(stat="identity") +
              ylab("Contribution to sentiment") + coord_flip()


data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE)

inaug_td <- tidy(inaug_dfm)
inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

inaug_tf_idf %>% group_by(document) %>% 
    filter(document %in% c('1861-Lincoln','1933-Roosevelt','1961-Kennedy','2009-Obama')) %>%
    top_n(10) %>% ungroup() %>%
    mutate(term = reorder_within(term, tf_idf, document)) %>%
    ggplot(aes(x = term, y = tf_idf, fill=document)) + 
    geom_bar(stat="identity") + scale_x_reordered() +
  facet_wrap(.~document, ncol = 2, scales = "free") + coord_flip()

year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>%
  complete(year, term, fill = list(count = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(count))


year_term_counts %>%
  filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
  ggplot(aes(year, count /year_total)) + geom_point() + geom_smooth() + 
  facet_wrap(~term, scales = "free_y") + scale_y_continuous(labels = scales::percent_format()) +
  ylab("% frequency of word in inaugural address")
  

ap_td %>% cast_dtm(document, term, count)

m <- ap_td %>%
  cast_sparse(document, term, count)

class(m)
typeof(m)
dim(m)
ncol(m)
nrow(m)

austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)





