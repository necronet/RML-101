library(broom)
library(glmnet)
library(yardstick)
library(doMC)
library(ggplot2)
library(forcats)
library(rsample)
library(R.oo)
registerDoMC(cores = detectCores())
source('./direcciones/preprocess.R')

classes <- c("MANAGUA","LEON")

address_data <- preprocess_original_file()

generate_binomial_model <- function(address_data, classes, freq_threshold = 5)  {
  REQUIRED_CLASSES = 2
  if (length(classes) != REQUIRED_CLASSES) {
    throw("Classes vector is required to be exactly 2")
  }
  
  # Modeling part
  addresses <- address_data %>% filter(Department %in% classes) %>% get_address()
  
  addresses_split <- addresses %>% select(doc_id)  %>% unique %>% initial_split()
  training_addresses <- training(addresses_split)
  testing_addresses <- testing(addresses_split)
  
  sparse_address_matrix <- addresses %>% group_by(word) %>% 
    filter(n() > freq_threshold) %>% count(doc_id, word) %>% inner_join(training_addresses) %>% 
    cast_sparse(doc_id, word, n)
  
  word_rownames <- as.integer(rownames(sparse_address_matrix))
  address_join <- tibble(doc_id = word_rownames) %>% left_join(addresses %>% select(doc_id, Department)) %>% unique
  is_1st_class <- address_join$Department == classes[1]
  
  list(model = cv.glmnet(sparse_address_matrix, is_1st_class,
                     family = "binomial",
                     parallel = TRUE, keep = TRUE), testing_addresses = testing_addresses, address_data = addresses)
}

binomial_model <- (address_data %>% generate_binomial_model(classes = c("MANAGUA", "LEON")))
plot(binomial_model$model)
plot(binomial_model$model$glmnet.fit)

coefs <- binomial_model$model$glmnet.fit %>%
  tidy() %>%
  filter(lambda == binomial_model$model$lambda.1se)

coefs %>%
  group_by(estimate > 0) %>%
  top_n(10, abs(estimate)) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(term, estimate), estimate, fill = estimate > 0)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip() +
  labs(
    x = NULL,
    title = "Most and least likely word tobe classified as in Managua"
  )

plot(binomial_model$model$glmnet.fit, xvar = "dev", label=TRUE)

intercept <- coefs %>%
  filter(term == "(Intercept)") %>%
  pull(estimate)


classifications <- address_data %>% filter(Department %in% c("MANAGUA", "LEON")) %>% get_address() %>%
  inner_join(binomial_model$testing_addresses) %>%
  inner_join(coefs, by = c("word" = "term")) %>%
  group_by(doc_id) %>%
  summarize(score = sum(estimate)) %>%
  mutate(probability = plogis(intercept + score))


classifications %>%
  left_join(binomial_model$address_data %>%
              select(Department, doc_id), by = "doc_id") %>%
  mutate(Department = as.factor(Department)) %>% roc_curve(Department, probability) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(
    color = "red",
    size = .5
  ) +
  geom_abline(
    lty = 3, alpha = 0.8,
    color = "gray50",
    size = 0.8
  ) +
  labs(
    title = "ROC curve for text classification using regularized regression",
    subtitle = "Predicting whether an address is from Managua or Leon"
  )


classifications %>%
  left_join(binomial_model$address_data %>%
              select(Department, doc_id), by = "doc_id") %>%
  mutate(Department = as.factor(Department)) %>%
  mutate(
    prediction = case_when(
      probability > 0.5 ~ "MANAGUA",
      TRUE ~ "LEON"
    ),
    prediction = as.factor(prediction)
  ) %>% filter(Department != prediction) %>%
  conf_mat(Department, prediction)

classifications %>%
  left_join(binomial_model$address_data %>%
              select(Department, doc_id), by = "doc_id") %>%
  mutate(Department = as.factor(Department)) %>%
  filter(
    probability < .20,
    Department == "MANAGUA"
  ) %>%
  sample_n(10) %>% inner_join(address_data %>% 
                                filter(Department %in% classes) %>%
                                mutate(doc_id = row_number()), by = c('doc_id')) %>%
  select(doc_id, probability, Address1, Address2)
