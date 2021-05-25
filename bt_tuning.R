# Random Forest tuning

# load packages
library(tidyverse)
library(tidymodels)
library(ranger)

# set seed
set.seed(234)

# load necessary items
load("data/setup.rda")

# define model
bt_model <- rand_forest(mode = "regression",
                        mtry = tune(),
                        min_n = tune(),
                        learn_rate = tune()) %>% 
  set_engine("xgboost", importance = "impurity")

# check tuning parameters
# parameters(bt_model)

# setup tuning grid
bt_params <- parameters(bt_model) %>% 
  update(mtry = mtry(range = c(2, 20)),
         learn_rate = learn_rate(range = c(-5,0.2))) # use 10 as upper bound instead of 14 vars

# define grid
bt_grid <- grid_regular(bt_params, levels = 5) # trying out every single combination of mtry and min_n

# workflow
bt_workflow <- 
  workflow() %>% 
  add_model(bt_model) %>% 
  add_recipe(recipe)

# tuning/fitting
bt_tune <- bt_workflow %>% 
  tune_grid(resamples = fold, grid = bt_grid)

# write out results & workflow
save(bt_tune, bt_workflow, file = "data/bt_tune.rda")

