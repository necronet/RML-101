# Principal Component Analysis
library(dplyr)
library(ggplot2)
library(h2o)

url <- "https://koalaverse.github.io/homlr/data/my_basket.csv"
my_basket <- readr::read_csv(url)
dim(my_basket)

summary(my_basket)

h2o.no_progress()
h2o.init(max_mem_size = "5g")
my_basket.h2o <- as.h2o(my_basket)

my_pca <- h2o.prcomp(
  training_frame = my_basket.h2o,
  pca_method = "GramSVD",
  k = ncol(my_basket.h2o), 
  transform = "STANDARDIZE", 
  impute_missing = TRUE,
  max_runtime_secs = 1000
)


my_pca@model$eigenvectors %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(pc1, reorder(feature, pc1))) +
  geom_point()


my_pca@model$eigenvectors %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(pc1, pc2, label = feature)) +
  geom_text()

