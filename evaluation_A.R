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

###############################################################
################# LOADING DATA BACK IN ########################
###############################################################

# loading files
load(file = "data/rf_tune_A.rda") # loading file back in

# autplots
rf_tune %>% 
  autoplot(metric = "rmse") # autoplot of tuned data

# Results Selection  -------------------------------------------------------------
# store info inside tibble
tune_results <- tibble( # setting up tibble of results
  model_type = "rf", # adding RF model
  tune_info = list(rf_tune), # gathering tuning info in list
  assessment_info = map(tune_info, collect_metrics), # looping through assessment info
  best_model = map(tune_info, ~ select_best(.x, metric = "rmse")) # pulling out best model
)

tune_results %>% 
  select(model_type, best_model) %>% # selecting best model exclusively
  unnest(best_model) # unnesting best model to view information

tune_results %>% 
  select(model_type, assessment_info) %>% # select assessment info
  unnest(assessment_info) %>% # unnest assessment info
  filter(.metric == "rmse") %>% # looking at RMSE
  arrange(mean) %>% # arranging by mean RMSE
  View()


# Finalizing Workflow -------------------------------------------------------------
# rf tuned workflow
rf_workflow_tuned <- rf_workflow %>% # looking at tuned workflow
  finalize_workflow(select_best(rf_tune, metric = "rmse")) # finalizing best model

# viewing results
rf_results <- fit(rf_workflow_tuned, train) # fitting best model
rf_results

# Predictions -------------------------------------------------------------
# predict
predictions <- predict(rf_results, new_data = test) %>% # predicting using new testing data
  bind_cols(Id = test$id) %>% # combining ID
  rename(Predicted = .pred) %>% # renaming to Predicted
  select(Id, Predicted) # selecting only ID and predicted

write_csv(predictions, file = "data/predictions_A.csv") # outputting results