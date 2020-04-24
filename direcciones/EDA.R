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

preprocess_original_file <- function(source_file = FILE_ADDRESS_1, target_file = TARGET_ADDRESS_FILE) {
  #Read oroginal file
  readr::read_csv("direcciones/data/clientes_rutas.csv") %>% 
    #Select only features that are interesting
    select(Department, Municipality, Address1, Address2, Neighborhood) %>% 
    # filter out NA values for department or municipality
    filter(!is.na(Department) & !is.na(Municipality)) %>% 
    # all data must be upper case for homogenity no special meaning between upper and lowercase
    mutate_all(str_to_upper) %T>%
    write_csv(target_file) 
}


address_filtered <- preprocess_original_file(FILE_ADDRESS_1, TARGET_ADDRESS_FILE) 



# Having the address1 and address2 together for unigram count
address_filtered %>% mutate(Address = paste(Address1, str_replace_na(Address2, ""))) %>% 
                     # Select only the newly created Address column and department
                     select(Department, Address) %>% 
                     # Getting all the words for each Address
                     unnest_tokens(word, Address) %>% group_by(Department, word) %>% 
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

# 
# library(lubridate)
#                     
#                 
# # S19 2018-2019
# pre.s19 <- ymd("2018-09-28") # beg of preseason
# # S20 2019-2020, included hiatus
# pre.s20 <- ymd("2019-09-30") # beg of preseason
# tibble(
#   date = seq.Date(from = ymd("2018-09-28"), to = today(), by = "days")) %>% 
#   filter(!is.instant(date))



