# loading packages
library(tidymodels)
library(tidyverse)
library(patchwork)
library(dplyr)

# setting seed
set.seed(3012)


# Exercise 1 --------------------------------------------------------------
# loading data
train <- read_csv("data/train.csv") %>% 
  janitor::clean_names()
test <- read_csv("data/test.csv") %>% 
  janitor::clean_names()


# Exercise 2 --------------------------------------------------------------
# pre-transform plot
p1 <- train %>%
  mutate(money_made_inv = log(money_made_inv)) %>% 
  ggplot(aes(x = money_made_inv)) +
  geom_density()
p1

# showing missingness
naniar::miss_var_summary(train)
skimr::skim_without_charts(train)


# Exercise 3 --------------------------------------------------------------
# folding data
fold <- vfold_cv(train, v= 5, repeats = 3, strata = money_made_inv)
fold



# Exercise 3.5 ------------------------------------------------------------
train %>% 
  group_by(emp_title) %>% 
  count() %>% 
  arrange(desc(n))
  arrange(desc(count()))


# Exercise 4 --------------------------------------------------------------
# setting up recipe
recipe <- recipe(
  money_made_inv ~ .,
  data = train
) %>% 
  step_rm(id, earliest_cr_line, last_credit_pull_d, addr_state) %>% 
  step_other(emp_title, sub_grade, emp_length) %>% 
  step_dummy(all_nominal()) %>%  # one hot encoding 
  step_normalize(all_predictors())

# prepping and baking data
recipe %>% 
  prep(train) %>% 
  bake(new_data = NULL) %>% 
  View()


# Exercise 5 --------------------------------------------------------------

# objects required for tuning
save(fold, recipe, split, file = "data/setup.rda")
