# Analyzing word and Document frequency
library(dplyr)
library(gutenbergr)
library(forcats)
library(stringr)

physics <- gutenberg_download(c(37729, 14725, 13476, 30155), 
                              meta_fields = "author")


physics_words <- physics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE)

plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  mutate(word = fct_reorder(word, tf_idf)) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan", 
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

plot_physics %>% group_by(author) %>% 
  top_n(15, tf_idf) %>%
  ungroup() %>% mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = author)) + 
  geom_col(show.legend=FALSE) +
  labs(x = NULL, y = "tf-idf") + facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()

physics %>% 
  filter(str_detect(text, "_k_")) %>% 
  select(text)

physics %>% 
  filter(str_detect(text, "RC")) %>% 
  select(text)

mystopwords <- tibble(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                               "fig", "file", "cg", "cb", "cm",
                               "ab", "_k", "_k_", "_x"))

physics_words <- anti_join(physics_words, mystopwords, by = "word")


plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  mutate(word = str_remove_all(word, "_")) %>%
  group_by(author) %>% 
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, author)) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))
ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered()


