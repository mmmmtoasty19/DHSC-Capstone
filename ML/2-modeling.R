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
  ,tune
)


require(ggplot2)  #this is needed for autoplot to work with workflows


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

final_rf_workflow <- rf_workflow %>%
  tune::finalize_workflow(rf_best_params)

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




# x-boost- class ----------------------------------------------------------

x_boost_rec <- r$recipe(ft4_dia ~ . , data = ds_train) %>%
  r$step_rm(FT4) %>%
  r$step_impute_bag(r$all_predictors()) %>%
  r$step_dummy(gender)

xgb_spec <-
  p$boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(),
               min_n = tune(), sample_size = tune(), trees = tune()) %>%
  p$set_engine("xgboost") %>%
  p$set_mode("classification")

xboost_wf <- wf$workflow() %>%
  wf$add_model(xgb_spec) %>%
  wf$add_recipe(x_boost_rec)

xboost_parms <- p$extract_parameter_set_dials(xgb_spec)

# takes around 6 hours to tune
# xboost_tune <- xboost_wf %>%
#     tune::tune_grid(
#     data_fold
#     ,grid = xboost_parms%>% d$grid_regular()
#     ,control = tune::control_grid(verbose = TRUE)
#   )


xboost_best_params <- readRDS(here::here("ML", "outputs", "xboosttune_class.rds")) %>%
  tune::select_best(metric = "accuracy")


final_xboost_wf <- xboost_wf %>%
  tune::finalize_workflow(xboost_best_params)

# fit training data to best model

final_xboost_fit <- p$fit(final_xboost_wf, ds_train)

final_xboost_predict <- ds_train %>%
  dplyr::select(ft4_dia) %>%
  dplyr::bind_cols(
    predict(final_xboost_fit, ds_train)
    ,predict(final_xboost_fit, ds_train, type = "prob")
  )

ys$accuracy(final_xboost_predict,truth = ft4_dia, estimate = .pred_class )

final_conf_xboost <- ys$conf_mat(final_xboost_predict, ft4_dia, .pred_class)


# fitting test data

class_test_results_boost <-
  final_xboost_fit %>%
  tune::last_fit(split = model_data_split)


ys$accuracy(class_test_results_boost %>% tune::collect_predictions()
            ,truth = ft4_dia, estimate = .pred_class )

class_test_result_conf_matrix <- ys$conf_mat(
  class_test_results_boost %>%  tune::collect_predictions()
  ,truth = ft4_dia
  ,estimate = .pred_class
)



# random forest regression ------------------------------------------------
#
reg_metrics <- ys$metric_set(ys$rmse, ys$rsq, ys$mae)

rf_reg_tune_model <- p$rand_forest(trees = tune(), mtry = tune(), min_n = tune()) %>%
  p$set_engine("ranger") %>% p$set_mode("regression")

rf_reg_recipe <- r$recipe(FT4 ~ . , data = ds_train) %>%
  r$step_rm(ft4_dia) %>%
  r$step_impute_bag(r$all_predictors())


rf_reg_workflow <- wf$workflow() %>%
  wf$add_model(rf_reg_tune_model) %>%
  wf$add_recipe(rf_reg_recipe)


rf_reg_param <- p$extract_parameter_set_dials(rf_reg_tune_model) %>%
  update(mtry = d$finalize(d$mtry(), ds_train))

data_fold_reg <- rsamp$vfold_cv(ds_train, v = 5)

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

final_rf_reg_fit <- p$fit(final_rf_reg_workflow, ds_train)


# predictions for training data

final_rf_reg_predict <-  ds_train %>%
  dplyr::select(FT4, TSH, ft4_dia) %>%
  dplyr::bind_cols(
    predict(final_rf_reg_fit, ds_train)
  ) %>%
  dplyr::mutate(
    ft4_dia_pred = dplyr::case_when(
      TSH > 4.2 & `.pred` < 0.93 ~ "Hypo"
      ,TSH > 4.2 & `.pred` > 0.93 ~ "Non-Hypo"
      ,TSH < 0.27 & `.pred` > 1.7  ~ "Hyper"
      ,TSH < 0.27 & `.pred` < 1.7  ~ "Non-Hyper"
    )
  ) %>%
  dplyr::mutate(dplyr::across(
    ft4_dia_pred
    , ~factor(., levels = c("Hypo", "Non-Hypo","Hyper", "Non-Hyper")
    )
  )
  )

ys$conf_mat(final_rf_reg_predict,truth = ft4_dia ,estimate = ft4_dia_pred)
ys$accuracy(final_rf_reg_predict,truth = ft4_dia, estimate = ft4_dia_pred)

reg_metrics(final_rf_reg_predict, truth = FT4, estimate = .pred)

ggplot(final_rf_reg_predict, aes(x = FT4, y = .pred)) +
  gp2$geom_abline(lty = 2) +
  gp2$geom_point(alpha = 0.5) +
  tune::coord_obs_pred()

# fitting test data

reg_test_results <-
  final_rf_reg_fit %>%
  tune::last_fit(split = model_data_split)

ds_reg_class_pred <- reg_test_results %>%
  tune::collect_predictions() %>%
  dplyr::select(-id, -.config) %>%
  dplyr::bind_cols(ds_test %>% dplyr::select(TSH, ft4_dia)) %>%
  dplyr::mutate(
    ft4_dia_pred = dplyr::case_when(
      TSH > 4.2 & `.pred` < 0.93 ~ "Hypo"
      ,TSH > 4.2 & `.pred` > 0.93 ~ "Non-Hypo"
      ,TSH < 0.27 & `.pred` > 1.7  ~ "Hyper"
      ,TSH < 0.27 & `.pred` < 1.7  ~ "Non-Hyper"
      )
    ) %>%
  dplyr::mutate(dplyr::across(
    ft4_dia_pred
    , ~factor(., levels = c("Hypo", "Non-Hypo","Hyper", "Non-Hyper")
    )
  )
  )

ys$accuracy(ds_reg_class_pred,truth = ft4_dia, estimate = ft4_dia_pred)
ys$conf_mat(ds_reg_class_pred,truth = ft4_dia ,estimate = ft4_dia_pred)

tune::collect_metrics(reg_test_results)

ggplot(reg_test_results %>% tune::collect_predictions() , aes(x = FT4, y = .pred)) +
  gp2$geom_abline(lty = 2) +
  gp2$geom_point(alpha = 0.5) +
  tune::coord_obs_pred()


# check orginal data

model_data %>%
  dplyr::mutate(tsh_level = ifelse(TSH > 4.2, "high", "low")) %>%
  dplyr::group_by(tsh_level, ft4_dia) %>%
  dplyr::summarise(
    n = n()
  ) %>%
  mutate(freq = n / sum(n))





