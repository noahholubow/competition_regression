# Random Forest tuning

# load packages
library(tidyverse)
library(tidymodels)
library(ranger)

# set seed
set.seed(123) # setting seed

# load necessary items
load("data/setup_B.rda") # loading in data

# define model
rf_model <- rand_forest(mode = "regression", # creating random forest model for regression
                        mtry = tune(), # tuning mtry
                        min_n = tune()) %>%  # tuning min_n
  set_engine("ranger") # using ranger engine for random forest

# setup tuning grid
rf_params <- parameters(rf_model) %>% # adding model to parameters
  update(mtry = mtry(range = c(2, 25))) # use 10 as upper bound instead of 14 vars

# define grid
rf_grid <- grid_regular(rf_params, levels = 5) # trying out every single combination of mtry and min_n

# workflow
rf_workflow <- 
  workflow() %>% # creating workflow
  add_model(rf_model) %>% # adding model to workflow
  add_recipe(recipe) # adding recipe to workflow

# tuning/fitting
rf_tune <- rf_workflow %>% # creating tuning object
  tune_grid(resamples = fold, grid = rf_grid) # tuning grid

# write out results & workflow
save(rf_tune, rf_workflow, file = "data/rf_tune_B.rda") # saving results

