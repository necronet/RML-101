library(openintro)
library(dplyr)
# Load ggplot2
library(ggplot2)

# Load data
data(email50)

# View the structure of the data
str(email50)

glimpse(email50)

table(email50$number)

email50_big <- email50 %>%
  filter(number == "big")

glimpse(email50_big)

# Table of the number variable
table(email50_big$number)

# Drop levels
email50_big$number_dropped <- droplevels(email50_big$number)

# Table of the number_dropped variable
table(email50_big$number_dropped)

# Calculate median number of characters: med_num_char
med_num_char <- median(email50$num_char)

# Create num_char_cat variable in email50
email50_fortified <- email50 %>%
  mutate(num_char_cat = ifelse(num_char < med_num_char, "bellow median", "at or above median"))

# Count emails in each category
email50_fortified %>%
  count(num_char_cat)

# Create number_yn column in email50
email50_fortified <- email50 %>%
  mutate(
    number_yn = case_when(
      # if number is "none", make number_yn "no"
      number=='none' ~ "no", 
      # if number is not "none", make number_yn "yes"
      number!='none' ~ "yes"  
    )
  )

# Visualize the distribution of number_yn
ggplot(email50_fortified, aes(x = number_yn)) +
  geom_bar()

# Scatterplot of exclaim_mess vs. num_char
ggplot(email50, aes(x = num_char, y = exclaim_mess, color = factor(spam))) + geom_point()


# Simpsons paradox
glimpse(UCBAdmissions)
ucb_admits <- as.data.frame(UCBAdmissions)
ucb_admission_counts <- ucb_admits %>% count(Gender, Admit)


ucb_admission_counts %>%
  # Group by gender
  group_by(Gender) %>%
  # Create new variable
  mutate(prop = n / sum(n)) %>%
  # Filter for admitted
  filter(Admit == "Admitted")

ucb_admission_counts <- ucb_admits %>%
  # Counts by department, then gender, then admission status
  count(Dept, Gender, Admit)

ucb_admission_counts  %>%
  # Group by department, then gender
  group_by(Dept, Gender) %>%
  # Create new variable
  mutate(prop = n / sum(n)) %>%
  # Filter for male and admitted
  filter(Admit == "Admitted", Gender == "Male")



# Sampling code in R
data(state)
us_regions <- as.data.frame(cbind(state.name,as.character(state.region)))
names(us_regions) <- c("state","region")

states_srs <- us_regions %>% sample_n(size = 8)
states_srs %>% count(region)

states_str <- us_regions %>%
  group_by(region) %>%
  sample_n(size = 8)

states_str %>% count(region)


# Case study 
download.file("http://www.openintro.org/stat/data/evals.RData", destfile = "evals.RData")
load("evals.RData")

# Inspect variable types
glimpse(evals)

evals$cls_students

# Recode cls_students as cls_type
evals_fortified <- evals %>%
  mutate(
    cls_type = case_when(
      cls_students <= 18 ~ "small",
      cls_students >= 19 & cls_students <= 59   ~ "midsize",
      cls_students >= 60 ~ "large"
    )
  )

# Scatterplot of score vs. bty_avg
ggplot(evals, aes(x = bty_avg, y = score)) + geom_point()


# Scatterplot of score vs. bty_avg colored by cls_type
ggplot(evals_fortified, aes(x = bty_avg, y = score, color = cls_type)) + geom_point()





