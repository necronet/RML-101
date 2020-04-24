# First ETL for the dataset at hand
library(readr)
library(dplyr)
library(stringr)
library(tidytext)
library(magrittr)
library(ggplot2)
library(forcats)
library(tidyr)

# Constant section

FILE_ADDRESS_1 <- "direcciones/data/clientes_rutas.csv"
TARGET_ADDRESS_FILE = "direcciones/data/address_filtered.csv"

str_replace_all("ENTRADA 1C. ABAJO, 3C. NORTE, 40VRS. ABAJO" , "\\d[cC]", "NC")

preprocess_original_file <- function(source_file = FILE_ADDRESS_1, target_file = TARGET_ADDRESS_FILE) {
  #Read oroginal file
  readr::read_csv("direcciones/data/clientes_rutas.csv") %>% 
    #treatment to remove commas and dots
    mutate_all(str_replace_all, pattern="[\\.,]", replacement = " ") %>%
    #remove white space on 1 c to 1c
    mutate_all(str_replace_all, pattern="(\\d)(\\s+)[cC]\\s+", replacement = "\\1") %>%
    #mutate_all(str_replace_all, pattern="\\d/\\d\\s+", replacement = "NC") %>%
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

address_filtered <- preprocess_original_file(FILE_ADDRESS_1, TARGET_ADDRESS_FILE) 

addresses <- function(address_filtered) {
  address_filtered %>% mutate(Address = paste(Address1, str_replace_na(Address2, ""))) %>% 
    # Select only the newly created Address column and department
    select(Department, Address) %>% 
    # Getting all the words for each Address
    unnest_tokens(word, Address)  
}

# Having the address1 and address2 together for unigram count
addresses(address_filtered) %>% group_by(Department, word) %>% 
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


addresses(address_filtered) %>% group_by(Department) %>% count(Department, word) %>% 
                                mutate(proportion = n / sum(n)) %>% select(-n) %>% 
                                pivot_wider(names_from = "Department", values_from = "proportion") %>%
                                pivot_longer(cols = c(`LEON`,`MASAYA`,`ESTELI`,`GRANADA`), names_to = "Department",values_to="proportion") %>% 
                      ggplot(aes(x = proportion, y = `MANAGUA`, color = abs(`MANAGUA` - proportion))) +
                      geom_abline(color = "gray40", lty = 2) +
                      geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
                      geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
                      scale_x_log10(labels = percent_format()) +
                      scale_y_log10(labels = percent_format()) +
                      scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
                      facet_wrap(~Department, ncol = 2) +
                      theme(legend.position="none") +
                      labs(y = "MANAGUA", x = NULL) +
                      ggtitle("Proportion of address in each department v Managua(Capital)")





  
  




