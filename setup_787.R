# loading packages
library(tidymodels)
library(tidyverse)
library(patchwork)
library(skimr)

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
skimmed <- skim_without_charts(train)
print(skimmed, strip_metadata = FALSE)

# correlation plot
corrplot::corrplot(cor(
  train %>% select(money_made_inv, annual_inc, avg_cur_bal, 
                   bc_util, dti, int_rate, loan_amnt, 
                   mort_acc, pub_rec, tot_coll_amt, tot_cur_bal, 
                   total_rec_late_fee)), method = "circle")

#summary
summary(train)

# graphs
train %>% 
  ggplot(aes(x = tot_coll_amt)) +
  geom_histogram()


# Exercise 3 --------------------------------------------------------------
# folding data
fold <- vfold_cv(train, v = 5, repeats = 3, strata = money_made_inv)
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
  step_rm(id, earliest_cr_line, last_credit_pull_d) %>% 
  step_novel(purpose) %>% 
  step_other(all_nominal(), -all_outcomes(), threshold = 0.09) %>% 
  step_YeoJohnson(annual_inc, avg_cur_bal, bc_util, mort_acc) %>% 
  step_log(annual_inc, avg_cur_bal, bc_util, mort_acc, base = 10) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>%  # one hot encoding 
  step_normalize(all_predictors(), -all_outcomes()) %>% 
  # step_interact(terms = ~tot_cur_bal:avg_cur_balance + mort_acc:avg_cur_bal) %>% 
  step_nzv(all_predictors(), -all_outcomes()) %>% 
  step_zv(all_predictors(), -all_outcomes())

# prepping and baking data
recipe %>%
  prep(train) %>%
  bake(new_data = NULL) %>%
  View()


# Exercise 5 --------------------------------------------------------------

# objects required for tuning
save(fold, recipe, split, file = "data/setup.rda")





###############################################################
################# LOADING DATA BACK IN ########################
###############################################################





set.seed(3012)

# loading files
load(file = "data/rf_tune.rda")

# autplots
rf_tune %>% 
  autoplot(metric = "rmse")


# Exercise 11 -------------------------------------------------------------
# selecting best metric

# store info inside tibble
tune_results <- tibble(
  model_type = "rf",
  tune_info = list(rf_tune),
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
  finalize_workflow(select_best(rf_tune, metric = "rmse"))

# viewing results
rf_results <- fit(rf_workflow_tuned, train)
rf_results

# Exercise 13 -------------------------------------------------------------
# predict
predictions <- predict(rf_results, new_data = test) %>% 
  bind_cols(Id = test$id) %>% 
  rename(Predicted = .pred) %>% 
  select(Id, Predicted)

write_csv(predictions, file = "data/predictions.csv")
