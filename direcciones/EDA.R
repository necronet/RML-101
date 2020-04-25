# First ETL for the dataset at hand
library(dplyr)
library(tidytext)
library(magrittr)
library(ggplot2)
library(forcats)
library(tidyr)
source('./direcciones/preprocess.R')

address_data <- preprocess_original_file() %>% get_address()

# Graph related to frequency of words per department
address_data %>% group_by(Department, word) %>% 
                     # counting frequency per word per department, sort med desc, and filter only those with more than 50 in frequency
                     summarise(n = n()) %>% top_n(10) %>% arrange(-n) %>% 
                     ungroup() %>% filter(n > 10) %>%
                     # pivot_wider(names_from = "word", values_from = "n") %>% filter(Department) 
                     ggplot(aes(x = reorder_within(word, n, Department), y = n, fill = word)) + 
                     geom_bar(stat="identity") + 
                     scale_x_reordered() +
                     coord_flip() +
                     facet_wrap(~Department, scales="free") + 
                     ggtitle("Distribution of words frequency on address per department") +
                     xlab("Unigrams per department") + ylab("Frequency")



# A relationship graph between words proportion and the capital
address_data %>% group_by(Department) %>% count(Department, word) %>% 
                                mutate(proportion = n / sum(n)) %>% select(-n) %>% 
                                pivot_wider(names_from = "Department", values_from = "proportion") %>%
                                pivot_longer(cols = c(`LEON`,`MASAYA`,`ESTELI`,`GRANADA`, `CHINANDEGA`, `RIVAS`), names_to = "Department",values_to="proportion") %>% 
                      ggplot(aes(x = proportion, y = `MANAGUA`, color = abs(`MANAGUA` - proportion))) +
                      geom_abline(color = "gray30", lty = 3) +
                      geom_jitter(alpha = 0.25, size = 2, width = 0.3, height = 0.3) +
                      geom_text(aes(label = word), check_overlap = TRUE, vjust = 1) +
                      scale_x_log10(labels = percent_format()) +
                      scale_y_log10(labels = percent_format()) +
                      scale_colour_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray50") +
                      facet_wrap(~Department, ncol = 2) +
                      theme(legend.position="none") +
                      labs(y = "MANAGUA", x = NULL) +
                      ggtitle("Proportion of address in each department v Managua(Capital)")


# Zipf's law proof
address_data %>% get_freq_by_rank(50) %>% filter(total > 50) %>%
  ggplot(aes(rank, freq_term, color = Department)) + 
  geom_abline(intercept = -1.0424 , slope = -0.5478, color = "gray50", linetype = 2) +
  geom_line(size = .5, alpha = 0.8) + 
  scale_x_log10() +
  scale_y_log10() + xlab("Rank") + ylab("Frequency term") + ggtitle("Zipf's law on Nicaragua address")

# What each address revelant word per department

address_data %>% get_freq_by_rank(50) %>% 
  bind_tf_idf(word, Department, n) %>% select(-total) %>% arrange(desc(tf_idf)) %>% group_by(Department) %>% 
  filter(Department %in% c("MANAGUA","LEON","ESTELI","MASAYA","GRANADA", "CHINANDEGA")) %>% top_n(15) %>% ungroup() %>%
  ggplot(aes(reorder_within(word, tf_idf, Department), tf_idf, fill=Department)) + geom_col(show.legend = FALSE) +
  facet_wrap(~Department, scales="free") + scale_x_reordered() + coord_flip() + xlab("Word per department") + ylab("TF-IDF")





preprocess_original_file() %>% address_ngram_count(ngram = 3)






  
