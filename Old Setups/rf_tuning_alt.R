# Random Forest tuning

# load packages
library(tidyverse)
library(tidymodels)
library(ranger)

# set seed
set.seed(123)

# load necessary items
load("data/setup_797.rda")

# define model
rf_model <- rand_forest(mode = "regression",
                        mtry = tune(),
                        min_n = tune()) %>% 
  set_engine("ranger")

# check tuning parameters
# parameters(rf_model)

# setup tuning grid
rf_params <- parameters(rf_model) %>% 
  update(mtry = mtry(range = c(2, 25))) # use 10 as upper bound instead of 14 vars

# define grid
rf_grid <- grid_regular(rf_params, levels = 5) # trying out every single combination of mtry and min_n

# workflow
rf_workflow <- 
  workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(recipe)

# tuning/fitting
rf_tune <- rf_workflow %>% 
  tune_grid(resamples = fold, grid = rf_grid)

# write out results & workflow
save(rf_tune, rf_workflow, file = "data/rf_tune_797.rda")

