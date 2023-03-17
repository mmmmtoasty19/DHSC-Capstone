# The following script is for training and testing Regression models
rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run.
cat("\014") # Clear the console


# load packages -----------------------------------------------------------

box::use(
  magrittr[`%>%`]
  ,here[here]
  ,readr
  ,gp2 = ggplot2[ggplot, aes]
  ,rsample
  ,r = recipes
  ,wf = workflows
  ,p = parsnip[tune]
  ,ys = yardstick
  ,d = dials
  ,rsamp = rsample
  ,tune
)



# globals -----------------------------------------------------------------

set.seed(070823) #set seed for reproducible research


# load-data ---------------------------------------------------------------

model_data <- readr$read_rds(here("ML","data-unshared","model_data.RDS"))


# split data --------------------------------------------------------------

model_data_split <- rsample$initial_split(
  model_data
  ,prop   = 0.80
  ,strata = ft4_dia
)

ds_train <- rsample$training(model_data_split)
ds_test  <- rsample$testing(model_data_split)

# verify distribution of data
table(ds_train$ft4_dia) %>% prop.table()
table(ds_test$ft4_dia) %>% prop.table()


ds_train <- ds_train %>% dplyr::select(-ft4_dia)
ds_test  <- ds_test %>% dplyr::select(-ft4_dia)

data_folds <- rsamp$vfold_cv(ds_train, repeats = 5)


# recipes ------------------------------------------------------------------


# Neural Net, KNN
normalized_rec <- r$recipe(FT4 ~ ., data = ds_train) %>%
  r$update_role(subject_id, new_role = "id") %>%
  r$update_role(charttime, new_role = "time") %>%
  r$step_impute_bag(r$all_predictors()) %>%
  r$step_BoxCox(r$all_numeric()) %>%
  r$step_corr(r$all_numeric_predictors()) %>%
  r$step_normalize(r$all_numeric())


# Random Forest and Boasted Tree
rf_rec <- r$recipe(FT4 ~ . , data = ds_train) %>%
  r$update_role(subject_id, new_role = "id") %>%
  r$update_role(charttime, new_role = "time") %>%
  r$step_impute_bag(r$all_predictors())




# models ------------------------------------------------------------------


nnet_spec <-
  p$mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  p$set_engine("nnet", MaxNWts = 2600) %>%
  p$set_mode("regression")


knn_spec <-
  p$nearest_neighbor(neighbors = tune(), dist_power = tune(), weight_func = tune()) %>%
  p$set_engine("kknn") %>%
  p$set_mode("regression")


rf_spec <-
  p$rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
  p$set_engine("ranger") %>%
  p$set_mode("regression")


xgb_spec <-
  p$boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(),
             min_n = tune(), sample_size = tune(), trees = tune()) %>%
  p$set_engine("xgboost") %>%
  p$set_mode("regression")



nnet_param <-
  nnet_spec %>%
  tune$extract_parameter_set_dials() %>%
  update(hidden_units = d$hidden_units(c(1, 27)))



# workflows ---------------------------------------------------------------

normalized <-
  workflowsets::workflow_set(
    preproc = list(normalized = normalized_rec),
    models = list(KNN = knn_spec, neural_network = nnet_spec)
  ) %>%
  workflowsets::option_add(param_info = nnet_param, id = "normalized_neural_network")

forests <-
  workflowsets::workflow_set(
    preproc = list(forests = rf_rec),
    models = list(RF = rf_spec, boosting = xgb_spec)
  )


all_workflows <-
  dplyr::bind_rows(normalized, forests) %>%
  dplyr::mutate(wflow_id = gsub("(forests_)|(normalized_)", "", wflow_id))
