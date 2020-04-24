# First ETL for the dataset at hand
library(readr)

address_filtered <- readr::read_csv("direcciones/data/address_filtered.csv")

address_filtered %>% mutate(Address = paste(Address1, str_replace_naAddress2)) %>% select(Address) %>% unnest_tokens(word, Address)