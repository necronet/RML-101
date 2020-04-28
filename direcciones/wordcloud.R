# R generate different word cloud for addresses
library(ggwordcloud)
source('./direcciones/preprocess.R')

stop_words_es <- stopwords::stopwords(language = "es")

addresses <- preprocess_original_file() %>% get_address() 


addresses %>% filter(Department %in% c('MANAGUA', 'LEON','ESTELI','CHINANDEGA')) %>% count(Department, word) %>%  
  filter(!word %in% stop_words_es) %>%
  group_by(Department) %>%
  mutate(proportion = n / sum(n)) %>% 
  arrange(-n) %>%
  top_n(40, wt = n) %>%
  mutate(angle = 90*sample(c(0, 1), n(), replace=TRUE, prob = c(80,20))) %>%
  ggplot(aes(label = word, size = proportion, angle = angle)) +
  geom_text_wordcloud_area(area_corr_power =  1, rm_outside = TRUE) +
  scale_size_area(max_size = 20) +
  facet_wrap(~Department) + theme_minimal()




  