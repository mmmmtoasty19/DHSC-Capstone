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
  ,p = parsnip
  ,ys = yardstick
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



# random forest -----------------------------------------------------------


rf_model <- p$rand_forest(trees = 1900) %>%
  p$set_engine("ranger") %>% p$set_mode("classification")

rf_recipe <- r$recipe(ft4_dia ~ . , data = ds_train) %>%
  r$update_role(subject_id, new_role = "id") %>%
  r$update_role(charttime, new_role = "time") %>%
  r$step_impute_bag(r$all_numeric())



rf_workflow <- wf$workflow() %>%
  wf$add_model(rf_model) %>%
  wf$add_recipe(rf_recipe)

rf_fit <- p$fit(rf_workflow, ds_train)

rf_predict <- ds_train %>%
  dplyr::select(ft4_dia) %>%
  dplyr::bind_cols(
    predict(rf_fit, ds_train)
    ,predict(rf_fit, ds_train, type = "prob")
    )

ys$accuracy(rf_predict, ft4_dia, .pred_class)
ys$conf_mat(rf_predict, ft4_dia, .pred_class)
