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


ds_train <- ds_train %>% dplyr::select(-ft4_dia)
ds_test  <- ds_test %>% dplyr::select(-ft4_dia)

data_folds <- rsamp$vfold_cv(ds_train, repeats = 5)



# recipes ------------------------------------------------------------------


# Neural Net, KNN
normalized_rec <- recipes::recipe(FT4 ~ ., data = ds_train) %>%
  recipes::step_impute_bag(recipes::all_predictors()) %>%
  # recipes::step_corr(recipes::all_numeric_predictors()) %>%
  recipes::step_normalize(recipes::all_numeric_predictors() , -anchor_age) %>%
  recipes::step_dummy(gender)


# Random Forest and Boasted Tree
rf_rec <- recipes::recipe(FT4 ~ . , data = ds_train) %>%
  recipes::step_impute_bag(recipes::all_predictors())

boost_rec <- recipes::recipe(FT4 ~ . , data = ds_train) %>%
  recipes::step_impute_bag(recipes::all_predictors()) %>%
  recipes::step_dummy(gender)


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
  p$rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>%
  p$set_engine("ranger") %>%
  p$set_mode("regression")


xgb_spec <-
  p$boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(),
               min_n = tune(), sample_size = tune(), trees = tune()) %>%
  p$set_engine("xgboost") %>%
  p$set_mode("regression")


svm_r_spec <-
  p$svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  p$set_engine("kernlab") %>%
  p$set_mode("regression")

svm_p_spec <-
  p$svm_poly(cost = tune(), degree = tune()) %>%
  p$set_engine("kernlab") %>%
  p$set_mode("regression")



nnet_param <-
  nnet_spec %>%
  tune$extract_parameter_set_dials() %>%
  update(hidden_units = d$hidden_units(c(1, 27)))


rf_param <-
  rf_spec %>%
  tune$extract_parameter_set_dials() %>%
  d$finalize(ds_train)




# workflows ---------------------------------------------------------------

normalized <-
  workflowsets::workflow_set(
    preproc = list(normalized = normalized_rec),
    models = list(
      SVM_radial = svm_r_spec,
      # SVM_poly = svm_p_spec,
      KNN = knn_spec,
      neural_network = nnet_spec)
  ) %>%
  workflowsets::option_add(param_info = nnet_param, id = "normalized_neural_network")

forest <-
  workflowsets::workflow_set(
    preproc = list(forests = rf_rec),
    models = list(RF = rf_spec)
  ) %>%
  workflowsets::option_add(param_info = rf_param, id = "forests_RF")

boost <-
  workflowsets::workflow_set(
    preproc = list(boost = boost_rec),
    models = list(boosting = xgb_spec)
  )

all_workflows <-
  dplyr::bind_rows(normalized, forest, boost) %>%
  dplyr::mutate(wflow_id = gsub("(forest_)|(normalized_)|(boost_)", "", wflow_id))



# workflow screening ------------------------------------------------------
num_cores <- parallel::detectCores() - 2
doParallel::registerDoParallel(cores = num_cores)

screen_workflows <- all_workflows %>%
  workflowsets::workflow_map(
    resamples = data_folds,
    verbose = TRUE
  )





# grid search -------------------------------------------------------------




grid_ctrl <-
  tune$control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )



grid_results <-
  all_workflows %>%
  workflowsets::workflow_map(
    seed = 070823
    ,resamples = data_folds
    ,grid = 25
    ,control = grid_ctrl
    ,verbose = TRUE
  )
