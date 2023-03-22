# The following script is for graphing of models
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

screen_workflows_reg <- readr::read_rds(here("ML","outputs","workflowscreen_reg.rds"))




# graphing ----------------------------------------------------------------

reg_results <- screen_workflows_reg %>%
  workflowsets::rank_results() %>%
  dplyr::filter(.metric == "rmse") %>%
  dplyr::select(model, .config, rmse = mean, rank)

#TODO Save this for paper
ggplot2::autoplot(
  screen_workflows_reg
  ,rank_metric = "rmse"
  ,metric = "rmse"
  ,select_best = TRUE
  ) +
  ggplot2::geom_text(ggplot2::aes(y = mean, label = wflow_id)
                     # ,angle = 90
                     ,hjust = -0.2
                     ) +
  ggplot2::theme_bw() +
  ggplot2::scale_color_manual(values = rep("black", times = 5)) +
  ggplot2::theme(legend.position = "none")





