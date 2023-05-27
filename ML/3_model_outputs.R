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
  ,workflowsets
)



# globals -----------------------------------------------------------------

set.seed(070823) #set seed for reproducible research


# load-data ---------------------------------------------------------------

screen_workflows_reg <- readr::read_rds(here("ML","outputs","workflowscreen_reg.rds"))
screen_workflows_class <- readr::read_rds(here("ML","outputs","workflowscreen_class.rds"))




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
  ggplot2::theme(legend.position = "none") +
  ggplot2::labs(
    title = "Regression Model Screening"
    ,y = "RMSE"
  ) +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))


gp2$ggsave(
  here("figures","reg_screen.emf")
  ,width  = 7
  ,height = 7
  ,dpi    = 300
  ,device = devEMF::emf
)
gp2$ggsave(
  here("figures","reg_screen.png")
  ,width  = 7
  ,height = 7
  ,dpi    = 300
)

class_results <- screen_workflows_class %>%
  workflowsets::rank_results()


ggplot2::autoplot(
  screen_workflows_class
  ,rank_metric = "roc_auc"
  ,metric = "roc_auc"
  ,select_best = TRUE
) +
  ggplot2::geom_text(ggplot2::aes(y = mean, label = wflow_id)
                     # ,angle = 90
                     ,hjust = -0.2
  ) +
  ggplot2::theme_bw() +
  ggplot2::scale_color_manual(values = rep("black", times = 5)) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::labs(
    title = "Classification Model Screening"
    ,y = "Accuracy"
  ) +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

gp2$ggsave(
  here("figures","class_screen.emf")
  ,width  = 7
  ,height = 7
  ,dpi    = 300
  ,device = devEMF::emf
)
gp2$ggsave(
  here("figures","class_screen.png")
  ,width  = 7
  ,height = 7
  ,dpi    = 300
)


# best results ------------------------------------------------------------

best_class_result <-
  screen_workflows_class %>%
  workflowsets::extract_workflow_set_result("forests_RF") %>%
  tune::select_best(metric = "accuracy")




