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


class_train <- ds_train %>% dplyr::select(-FT4)  # training data for classification models
reg_train <- ds_train %>% dplyr::select(-ft4_dia) # training data for reg models predicting result

# random forest classification -----------------------------------------------------------

# base model - No Hyper Tuning

rf__base_model <- p$rand_forest() %>%
  p$set_engine("ranger") %>% p$set_mode("classification")

rf_recipe <- r$recipe(ft4_dia ~ . , data = class_train) %>%
  r$update_role(subject_id, new_role = "id") %>%
  r$update_role(charttime, new_role = "time") %>%
  r$step_impute_bag(r$all_predictors())


rf_workflow <- wf$workflow() %>%
  wf$add_model(rf__base_model) %>%
  wf$add_recipe(rf_recipe)

rf_base_fit <- p$fit(rf_workflow, class_train)

rf_predict <- class_train %>%
  dplyr::select(ft4_dia) %>%
  dplyr::bind_cols(
    predict(rf_base_fit, class_train)
    ,predict(rf_base_fit, class_train, type = "prob")
    )

conf_mat_rf <- ys$conf_mat(rf_predict, ft4_dia, .pred_class)



rf_pred <- dplyr::select(class_train, -ft4_dia, -subject_id, -charttime)

rf_tuning_model <- p$rand_forest(trees = tune(), mtry = tune(), min_n = tune()) %>%
  p$set_engine("ranger") %>% p$set_mode("classification")

rf_param <- p$extract_parameter_set_dials(rf_tuning_model)

rf_param <- rf_param %>% update(mtry = d$finalize(d$mtry(), rf_pred))

data_fold <- rsamp$vfold_cv(class_train, v = 5)

rf_workflow <- wf$update_model(rf_workflow, rf_tuning_model)

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

final_rf_workflow <- rf_workflow %>%
  tune::finalize_workflow(rf_best_params)

final_rf_fit <- p$fit(final_rf_workflow, class_train)

final_rf_predict <- class_train %>%
  dplyr::select(ft4_dia) %>%
  dplyr::bind_cols(
    predict(final_rf_fit, class_train)
    ,predict(final_rf_fit, class_train, type = "prob")
  )

final_conf_rf <- ys$conf_mat(final_rf_predict, ft4_dia, .pred_class)


# random forest regression ------------------------------------------------

reg_metrics <- ys$metric_set(ys$rmse, ys$rsq, ys$mae)

rf_base_reg_model <- p$rand_forest() %>%
  p$set_engine("ranger") %>% p$set_mode("regression")

rf_reg_recipe <- r$recipe(FT4 ~ . , data = reg_train) %>%
  r$update_role(subject_id, new_role = "id") %>%
  r$update_role(charttime, new_role = "time") %>%
  r$step_impute_bag(r$all_predictors())


rf_reg_workflow <- wf$workflow() %>%
  wf$add_model(rf_base_reg_model) %>%
  wf$add_recipe(rf_reg_recipe)

rf_base_reg_fit <- p$fit(rf_reg_workflow, reg_train)

rf_reg_predict <- reg_train %>%
  dplyr::select(FT4) %>%
  dplyr::bind_cols(
    predict(rf_base_reg_fit, reg_train)
  )

reg_metrics(rf_reg_predict, truth = FT4, estimate = .pred)

rf_reg_tune_model <- p$rand_forest(trees = tune(), mtry = tune(), min_n = tune()) %>%
  p$set_engine("ranger") %>% p$set_mode("regression")

rf_reg_pred <- dplyr::select(reg_train, -FT4, -subject_id, -charttime)

rf_reg_param <- p$extract_parameter_set_dials(rf_reg_tune_model) %>%
  update(mtry = d$finalize(d$mtry(), rf_reg_pred))z

data_fold_reg <- rsamp$vfold_cv(reg_train, v = 5)

# takes around 1 hr to run grid search.  saving best params manaually
rf_reg_tune <- rf_reg_workflow %>%
    tune::tune_grid(
    data_fold
    ,grid = rf_reg_param %>% d$grid_regular()
  )



