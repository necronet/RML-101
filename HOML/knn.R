library(dplyr)
library(ggplot2)
library(rsample)
library(recipes)
library(caret) 
library(dslabs)
library(AmesHousing)

set.seed(11052019)

sample_rate <- 0.75

attrit <- attrition %>% mutate_if(is.ordered, factor, ordered = FALSE)
churn_split <- initial_split(attrit, prop = .7, strata = "Attrition")
churn_train <- training(churn_split)

ames <- AmesHousing::make_ames()
split_1 <- initial_split(ames, prop = sample_rate)
ames_training <- training(split_1)
ames_testin <- testing(split_1)

(two_houses <- ames_training[1:2, c("Gr_Liv_Area", "Year_Built")])

dist(two_houses, method = "euclidean")
dist(two_houses, method = "manhattan")

ggplot(two_houses, aes(x=Gr_Liv_Area, y=Year_Built)) + geom_point() + geom_line(linetype=4) 

blueprint <- recipe(Attrition ~ . , data = churn_train) %>%
             step_nzv(all_nominal()) %>%
             step_integer(contains("Satisfaction")) %>%
             step_integer(WorkLifeBalance) %>%
             step_integer(JobInvolvement) %>%
             step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
             step_center(all_numeric(), -all_outcomes()) %>%
             step_scale(all_numeric(), -all_outcomes())


cv <- trainControl( method = "repeatedcv", number = 10, repeats = 5, 
                    classProbs = TRUE, summaryFunction = twoClassSummary,
                    verboseIter = TRUE)

hyper_grid <- expand.grid(k = floor(seq(1, nrow(churn_train)/3, length.out = 20)))

knn_grid <- train(blueprint, data = churn_train, method = "knn", trControl = cv,
                  tuneGrid = hyper_grid, metric = "ROC")

ggplot(knn_grid)


mnist <- dslabs::read_mnist()
names(mnist)

index <- sample(nrow(mnist$train$images), size = 10000)
mnist_x <- mnist$train$images[index, ]
mnist_y <- factor(mnist$train$labels[index])

# Couldn't make the provided example worked so this is an alternative solution less optimal.
mnist_x %>% as.data.frame() %>% sapply(sd) %>% as.vector %>% as.data.frame(x = . ) %>% 
  ggplot(aes(x = .)) + geom_histogram(binwidth = 1)


colnames(mnist_x) <- paste0("V", 1:ncol(mnist_x))
# Removing near zero variance
nzv <- nearZeroVar(mnist_x)
index <- setdiff(1:ncol(mnist_x), nzv)
mnist_x <- mnist_x[, index]


# create a simple train-validate resampling method
cv <- trainControl(
  method = "LGOCV", 
  p = 0.7,
  number = 1,
  savePredictions = TRUE
)

# create a hyperparameter grid search
hyper_grid <- expand.grid(k = seq(3, 25, by = 2))

# execute grid search
knn_mnist <- train(
  mnist_x,
  mnist_y,
  method = "knn",
  tuneGrid = hyper_grid,
  preProc = c("center", "scale"),
  trControl = cv
)

ggplot(knn_mnist)








