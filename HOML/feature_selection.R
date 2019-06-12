library(recipes)
library(visdat)
# Dataset dependencies
library(AmesHousing)

ames <- AmesHousing::make_ames()
churn <- rsample::attrition


# Counting NA values 
sum(is.na(AmesHousing::ames_raw))

# A graphic view of data with missing values

AmesHousing::ames_raw %>% is.na() %>% reshape2::melt() %>%
  ggplot(aes(Var2, Var1, fill=value)) + geom_raster() + 
  coord_flip() + scale_y_continuous(NULL, expand = c(0, 0)) +
  scale_fill_grey(name = "", labels = c("Present", "Missing") ) +
  xlab("Observation") + theme(axis.text.y = element_text(size=4))

# Considering that Garage_xx actually has information when is None identify 
# houses with no garage
AmesHousing::ames_raw %>% 
  filter(is.na(`Garage Type`)) %>% 
  select(`Garage Type`, `Garage Cars`, `Garage Area`)

# Visualizing missing data through with visdat library
vis_miss(AmesHousing::ames_raw, cluster=TRUE)


# Imputation techniques

