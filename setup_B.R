# loading packages
library(tidymodels)
library(tidyverse)
library(patchwork)
library(skimr)

# setting seed
set.seed(3012)



# Loading data ------------------------------------------------------------
# loading data
train <- read_csv("data/train.csv") %>% 
  janitor::clean_names() # creating training data
test <- read_csv("data/test.csv") %>% 
  janitor::clean_names() # creating testing data



# EDA ---------------------------------------------------------------------
# pre-transform plot
p1 <- train %>%
  mutate(money_made_inv = log(money_made_inv)) %>% # using log transformation with base 10
  ggplot(aes(x = money_made_inv)) + # setting up ggplot to view distribution
  geom_density() # using density plot
p1

# showing missingness
naniar::miss_var_summary(train) # looking as missing variable summary
skimmed <- skim_without_charts(train) # skimming without charts
print(skimmed, strip_metadata = FALSE) # printing skim information

# correlation plot
corrplot::corrplot(cor( # looking at correlation plot
  train %>% select(money_made_inv, annual_inc, avg_cur_bal, # adding numeric variables
                   bc_util, dti, int_rate, loan_amnt, 
                   mort_acc, pub_rec, tot_coll_amt, tot_cur_bal, 
                   total_rec_late_fee)), method = "circle")

#summary
summary(train) # looking at summary of training set

# graphs
train %>% 
  ggplot(aes(x = tot_coll_amt)) + # setting up plot of training data for total collection amount
  geom_histogram() # histogram of taining data

train %>% 
  group_by(emp_title) %>% # grouping by employee type
  count() %>% # counting number of employees per type
  arrange(desc(n)) # arranging by highest to lowest counts



# Folding --------------------------------------------------------------
# folding data
fold <- vfold_cv(train, v = 5, repeats = 3, strata = money_made_inv) # setting up folding with 5 folds and 3 repeats with stratification around money_made_inv
fold

# Recipe --------------------------------------------------------------
# setting up recipe
recipe <- recipe( # setting up recipe
  money_made_inv ~ ., # regressing money made on all variables
  data = train # using training data
) %>% 
  step_rm(id, earliest_cr_line, last_credit_pull_d) %>% # removing unusable/undesirable variables
  step_novel(purpose) %>% # used to handle new levels of purpose in testing data
  step_other(all_nominal(), -all_outcomes(), threshold = 0.1) %>% # binning sparse data for all nominal variables
  step_YeoJohnson(annual_inc, avg_cur_bal, bc_util, mort_acc) %>% # trying to make data more normal
  step_log(annual_inc, avg_cur_bal, bc_util, mort_acc, base = 10) %>% # also trying to make data more normal
  step_dummy(all_nominal(), -all_outcomes()) %>%  # creating dummy variables for all nominal data
  step_normalize(all_predictors(), -all_outcomes()) %>% # normalizing all predictors
  step_zv(all_predictors(), -all_outcomes()) # removing variables that have no variance

# prepping and baking data
recipe %>%
  prep(train) %>% # prepping data
  bake(new_data = NULL) %>% # baking data
  View()


# Exercise 5 --------------------------------------------------------------

# objects required for tuning
save(fold, recipe, split, file = "data/setup_B.rda") # saving recipe, folds, splits in new file for training

