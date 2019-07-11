set.seed(11052019)
library(recipes)
library(glmnet)
library(caret)
library(vip)
library(rsample)
library(AmesHousing)

ames <- AmesHousing::make_ames()
sample_rate <- .7

# Splittin data with random sample but with rsample package
split_1 <- initial_split(ames, prop = sample_rate)
ames_train <- training(split_1)
ames_test <- testing(split_1)

ggplot(ames_train, aes(x=Gr_Liv_Area, y=Sale_Price)) + 
  geom_smooth(se = FALSE, method = 'lm') +
  geom_point(size = 1, alpha = 0.4) +
  scale_y_continuous("Sale Price", labels = scales::dollar, breaks = seq(0, 600000, by = 100000)) 

# Generate model matrix
X <- model.matrix(Sale_Price ~ ., ames_train)[, -1]
Y <- log(ames_train$Sale_Price)

ridge <- glmnet(x = X, y = Y, alpha = 0)
plot(ridge, xvar = "lambda")

ridge$lambda %>% head()
coef(ridge)[c("Latitude", "Overall_QualVery_Excellent"), 100]
coef(ridge)[c("Latitude", "Overall_QualVery_Excellent"), 1]  


lasso <- glmnet(x = X, y = Y, alpha = 1)
plot(lasso, xvar = "lambda")


# cross K validation

ridge <- cv.glmnet(
  x = X,
  y = Y,
  alpha = 0
)

lasso <- cv.glmnet(
  x = X,
  y = Y,
  alpha = 1
)


par(mfrow = c(1, 2))
plot(ridge, main = "Ridge penalty\n\n")
plot(lasso, main = "Lasso penalty\n\n")


min(ridge$cvm)
ridge$lambda.min
ridge$cvm[ridge$lambda == ridge$lambda.1se]

min(lasso$cvm)
lasso$lambda.min
lasso$cvm[lasso$lambda == lasso$lambda.1se]


ridge_min <- glmnet(
  x = X,
  y = Y,
  alpha = 0
)

# Lasso model
lasso_min <- glmnet(
  x = X,
  y = Y,
  alpha = 1
)

par(mfrow = c(1, 2))
# plot ridge model
plot(ridge_min, xvar = "lambda", main = "Ridge penalty\n\n")
abline(v = log(ridge$lambda.min), col = "red", lty = "dashed")
abline(v = log(ridge$lambda.1se), col = "blue", lty = "dashed")

# plot lasso model
plot(lasso_min, xvar = "lambda", main = "Lasso penalty\n\n")
abline(v = log(lasso$lambda.min), col = "red", lty = "dashed")
abline(v = log(lasso$lambda.1se), col = "blue", lty = "dashed")


# grid search across 
cv_glmnet <- train(
  x = X,
  y = Y,
  method = "glmnet",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE),
  tuneLength = 10
)

# model with lowest RMSE
cv_glmnet$bestTune

ggplot(cv_glmnet)

pred <- predict(cv_glmnet, X)

# compute RMSE of transformed predicted
RMSE(exp(pred), exp(Y))

# Feature interpretation

vip(cv_glmnet, num_features = 20, bar = FALSE)
