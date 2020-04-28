# dplyr example with accross function
# This requires dplyr 1.0 as today (April 28 th has yet to be release)
# Based on https://drkeithmcnulty.com/2020/04/06/what-you-need-to-know-about-dplyr-1-0-0-part-1/
devtools::install_github("tidyverse/dplyr")

library(dplyr)

mtcars %>% group_by(cyl) %>% summarise(across(c('mpg','hp'), mean))


storms %>% 
  dplyr::mutate(across(is.character, as.factor)) %>% 
  dplyr::select(name, status)


storms %>% 
  dplyr::select(is.character)



mtcars %>% 
  group_by(cyl) %>% 
  summarise(across(c("mpg", "hp"), list(mean = mean, median = median, sd = sd))) 


mtcars %>% 
  group_by(cyl) %>% 
  summarise(across(c("mpg", "hp"), 
                   list(mean = mean, median = median, sd = sd), 
                   .names = "{col}_{fn}_summ")) 


mtcars %>% 
  group_by(cyl) %>% 
  summarise(across(c("mpg", "hp"), 
                   list(mean = ~mean(.x, na.rm = T), 
                        median = ~median(.x, na.rm = T), 
                        sd = ~sd(.x, na.rm = T)), 
                   .names = "{col}_{fn}_summ")) 



mtcars %>% 
  group_by(cyl) %>% 
  summarise(across(mpg, 
                   list(minus_sd = ~(mean(.x) - sd(.x)), 
                        mean = mean, 
                        plus_sd = ~(mean(.x) + sd(.x)))
  )) 



WorldPhones %>% 
  as.data.frame() %>% 
  rowwise() %>% 
  mutate(mean = mean(c_across(N.Amer:Mid.Amer), na.rm = TRUE))

starwars %>% 
  rowwise() %>% 
  mutate(
    stuff_they_own = length(c_across(c("vehicles", "starships")))
  ) %>% 
  select(name, stuff_they_own) 




