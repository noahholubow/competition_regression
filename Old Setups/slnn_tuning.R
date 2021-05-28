# Single Layer Neural Network tuning ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(tictoc)

set.seed(589)

# load required objects ----
load("data/setup.rda")


# Define model ----
slnn_model <- mlp(mode = "regression",
                  hidden_units = tune(),
                  penalty = tune()) %>% 
  set_engine("nnet")

# set-up tuning grid ----
slnn_params <- parameters(slnn_model)

# define tuning grid
slnn_grid <- grid_regular(slnn_params, levels = 5)

# workflow ----
slnn_workflow <- 
  workflow() %>% 
  add_model(slnn_model) %>% 
  add_recipe(recipe)

# Tuning/fitting ----
tic("SLNN Model")
# Pace tuning code in hear
slnn_tune <- slnn_workflow %>% 
  tune_grid(resamples = fold, grid = slnn_grid)

# Write out results & workflow
save(slnn_tune, slnn_workflow, file = "data/slnn_tune.rda")
