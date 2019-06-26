# Helper packages
library(dplyr)
library(caret)
library(ggplot)
library(AmesHousing)
library(rsample)
library(broom)
library(gridExtra)
library(vip)

set.seed(11052019)

ames <- AmesHousing::make_ames()

# Config variables
sample_rate <- .7

# Splittin data with random sample but with rsample package
split_1 <- initial_split(ames, prop = sample_rate)
train_ames <- training(split_1)
test_ames <- testing(split_1)


ols_model1 <- lm(Sale_Price ~ Gr_Liv_Area, data = train_ames)
summary(ols_model1)

predicted <-predict(ols_model1)
ggplot(ols_model1$model, aes(x=Gr_Liv_Area, y=Sale_Price)) + 
  stat_smooth(method='lm', col = 'red', se = FALSE) + 
  geom_segment(aes(xend = Gr_Liv_Area, yend=predicted), alpha = 0.3, col = 'red') + geom_point(alpha = 0.8) + 
  geom_point(aes(y = predicted), shape = 1) + theme_bw()

sigma(ols_model1) 

confint(ols_model1, level = 0.95)

# Multivariate OLS
ols_model2 <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built, data = train_ames)
# Alternatively update ols_model1 (ols_model2 <- update(ols_model1, . ~ . + Year_Built))

# With interaction between terms
ols_model3 <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built + Gr_Liv_Area : Year_Built, data = train_ames)

# TODO: ask how contour plots are built in the guide


# Including all paramters
ols_model4 <- lm(Sale_Price ~ ., data = train_ames) 

broom::tidy(ols_model4)


cv_ols_model1 <- caret::train(form = Sale_Price ~ Gr_Liv_Area,
                       data = train_ames, method = 'lm',
                       trControl = trainControl(method = 'cv', number = 10))

cv_ols_model2 <- caret::train(form = Sale_Price ~ Gr_Liv_Area + Year_Built,
                              data = train_ames, method = 'lm',
                              trControl = trainControl(method = 'cv', number = 10))

cv_ols_model3 <- caret::train(form = Sale_Price ~ .,
                              data = train_ames, method = 'lm',
                              trControl = trainControl(method = 'cv', number = 10))


summary(resamples(list(
  model1 = cv_ols_model1, 
  model2 = cv_ols_model2, 
  model3 = cv_ols_model3
)))


# plotting linear relationship

p1 <- ggplot(train_ames, aes( x = Gr_Liv_Area, y = Sale_Price)) + 
  geom_point(size = 1, alpha = 0.4) +
  geom_smooth(se = FALSE, method = 'gam', formula = y ~ s(x, bs = "cs")) + 
  scale_y_continuous("Sale Price", labels = scales::dollar) +
  xlab("Year Built") + ggtitle("Non transformed variables a \nnon linear relationship")

p2 <- ggplot(train_ames, aes( x = Gr_Liv_Area, y = Sale_Price)) + 
  geom_point(size = 1, alpha = 0.4) +
  geom_smooth(se = FALSE, method = 'lm') + 
  scale_y_log10("Sale Price", labels = scales::dollar, breaks = seq(0, 400000, by = 100000)) +
  xlab("Year Built") + ggtitle("Transformed variables a \nAlmost linear relationship")

gridExtra::grid.arrange(p1, p2, nrow = 1)

# Variance constant amog residual asumption for LS

df1 <- broom::augment(cv_ols_model1$finalModel, data = train_ames)

p1 <- ggplot(df1, aes(.fitted, .resid)) + geom_point(size = 1, alpha = 0.35)

df2 <- broom::augment(cv_ols_model3$finalModel, data = train_ames)
p2 <- ggplot(df2, aes(.fitted, .resid)) + geom_point(size = 1, alpha = 0.35)
gridExtra::grid.arrange(p1, p2, nrow = 1)

# No auto correlation assumption between errors

df1 <- mutate(df1, id = row_number())
df2 <- mutate(df2, id = row_number())

p1 <- ggplot(df1, aes(id, .resid)) + geom_point(size = 1, alpha = 0.4)
p2 <- ggplot(df2, aes(id, .resid)) + geom_point(size = 1, alpha = 0.4)
gridExtra::grid.arrange(p1, p2, nrow = 1)

# Multicollinearity

summary(cv_ols_model3) %>%
  broom::tidy() %>%
  filter(term %in% c("Garage_Area", "Garage_Cars"))



mod_wo_Garage_Area <- train(
  Sale_Price ~ ., 
  data = select(train_ames, -Garage_Area), 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

summary(mod_wo_Garage_Area) %>%
  broom::tidy() %>%
  filter(term == "Garage_Cars")


# Principal component regression
set.seed(123)
cv_model_pcr <- train(
  Sale_Price ~ ., 
  data = train_ames, 
  method = "pcr",
  trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE),
  preProcess = c("nzv", "center", "scale"),
  tuneLength = 25
)

cv_model_pcr$bestTune
ggplot(cv_model_pcr)

cv_model_pls <- train(
  Sale_Price ~ ., 
  data = train_ames, 
  method = "pls",
  trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 20
)

# model with lowest RMSE
cv_model_pls$bestTune
##    ncomp
## 10    10

# plot cross-validated RMSE
ggplot(cv_model_pls)



vip(cv_model_pls, num_features = 20, method = "model")