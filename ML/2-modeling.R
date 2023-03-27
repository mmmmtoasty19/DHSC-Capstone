# Random Forests are the best models for both types finalize grid searchs
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
)



# globals -----------------------------------------------------------------

set.seed(070823) #set seed for reproducible research


# load-data ---------------------------------------------------------------

model_data <- readr$read_rds(here("ML","data-unshared","model_data.RDS")) %>%
  dplyr::select(-subject_id, -charttime)


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

# random forest classification -----------------------------------------------------------

# base model - No Hyper Tuning

rf_recipe <- r$recipe(ft4_dia ~ . , data = ds_train) %>%
  r$step_rm(FT4) %>%
  r$step_impute_bag(r$all_predictors())

rf_tuning_model <- p$rand_forest(trees = tune(), mtry = tune(), min_n = tune()) %>%
  p$set_engine("ranger") %>% p$set_mode("classification")


rf_workflow <- wf$workflow() %>%
  wf$add_model(rf_tuning_model) %>%
  wf$add_recipe(rf_recipe)


rf_param <- p$extract_parameter_set_dials(rf_tuning_model)

rf_param <- rf_param %>% update(mtry = d$finalize(d$mtry(), ds_train))

data_fold <- rsamp$vfold_cv(ds_train, v = 5)



# takes around 1 hr to run grid search.  saving best params manaually
# rf_tune <- rf_workflow %>%
#     tune::tune_grid(
#     data_fold
#     ,grid = rf_param %>% d$grid_regular()
#   )

rf_best_params <- tibble::tibble(
  mtry = 8
  ,trees = 2000
  ,min_n = 2
)

rf_best_params_screen <-
  tibble::tibble(
    mtry = 7
    ,trees = 763
    ,min_n = 15
  )

final_rf_workflow <- rf_workflow %>%
  tune::finalize_workflow(rf_best_params_screen)

# Final Fit training data

final_rf_fit <- p$fit(final_rf_workflow, ds_train)

final_rf_predict <- ds_train %>%
  dplyr::select(ft4_dia) %>%
  dplyr::bind_cols(
    predict(final_rf_fit, ds_train)
    ,predict(final_rf_fit, ds_train, type = "prob")
  )

ys$accuracy(final_rf_predict,truth = ft4_dia, estimate = .pred_class )

final_conf_rf <- ys$conf_mat(final_rf_predict, ft4_dia, .pred_class)

# fitting test data

class_test_results <-
  final_rf_fit %>%
  tune::last_fit(split = model_data_split)

class_test_result_conf_matrix <- ys$conf_mat(
  class_test_results %>%  tune::collect_predictions()
  ,truth = ft4_dia
  ,estimate = .pred_class
  )



# random forest regression ------------------------------------------------
#
reg_metrics <- ys$metric_set(ys$rmse, ys$rsq, ys$mae)

rf_reg_tune_model <- p$rand_forest(trees = tune(), mtry = tune(), min_n = tune()) %>%
  p$set_engine("ranger") %>% p$set_mode("regression")

rf_reg_recipe <- r$recipe(FT4 ~ . , data = reg_train) %>%
  r$step_rm(ft4_dia) %>%
  r$step_impute_bag(r$all_predictors())


rf_reg_workflow <- wf$workflow() %>%
  wf$add_model(rf_reg_tune_model) %>%
  wf$add_recipe(rf_reg_recipe)


rf_reg_param <- p$extract_parameter_set_dials(rf_reg_tune_model) %>%
  update(mtry = d$finalize(d$mtry(), reg_train))

data_fold_reg <- rsamp$vfold_cv(reg_train, v = 5)

# takes around 1 hr to run grid search.  saving best params manaually
# rf_reg_tune <- rf_reg_workflow %>%
#     tune::tune_grid(
#     data_fold_reg
#     ,grid = rf_reg_param %>% d$grid_regular()
#   )

rf_reg_best_params <- tibble::tibble(
  mtry = 8
  ,trees = 1000
  ,min_n = 2
)

final_rf_reg_workflow <- rf_reg_workflow %>%
  tune::finalize_workflow(rf_reg_best_params)

final_rf_reg_fit <- p$fit(final_rf_reg_workflow, reg_train)


#
final_rf_reg_predict <-  reg_train %>%
  dplyr::select(FT4) %>%
  dplyr::bind_cols(
    predict(final_rf_reg_fit, reg_train)
  )

reg_metrics(final_rf_reg_predict, truth = FT4, estimate = .pred)


reg_test_results <-
  final_rf_reg_workflow %>%
  tune::last_fit()




