# loading packages
library(tidymodels)
library(tidyverse)
library(patchwork)
library(skimr)

# setting seed
set.seed(3012)

###############################################################
################# LOADING DATA BACK IN ########################
###############################################################

# loading files
load(file = "data/rf_tune_B.rda")

# autplots
rf_tune_B %>% 
  autoplot(metric = "rmse")

# Exercise 11 -------------------------------------------------------------
# selecting best metric

# store info inside tibble
tune_results <- tibble(
  model_type = "rf",
  tune_info = list(rf_tune_B),
  assessment_info = map(tune_info, collect_metrics),
  best_model = map(tune_info, ~ select_best(.x, metric = "rmse"))
)

tune_results %>% 
  select(model_type, best_model) %>% 
  unnest(best_model)

tune_results %>% 
  select(model_type, assessment_info) %>% 
  unnest(assessment_info) %>% 
  filter(.metric == "rmse") %>% 
  arrange(mean) %>% 
  View()


# Exericse 12 -------------------------------------------------------------
# rf tuned workflow
rf_workflow_tuned <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tune_B, metric = "rmse"))

# viewing results
rf_results <- fit(rf_workflow_tuned, train)
rf_results

# Exercise 13 -------------------------------------------------------------
# predict
predictions <- predict(rf_results, new_data = test) %>% 
  bind_cols(Id = test$id) %>% 
  rename(Predicted = .pred) %>% 
  select(Id, Predicted)

write_csv(predictions, file = "data/predictions_B.csv")