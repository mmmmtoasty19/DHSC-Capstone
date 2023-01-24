rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run.
cat("\014") # Clear the console


# load packages -----------------------------------------------------------

box::use(
  magrittr[`%>%`]
  ,here[here]
  ,dplyr
  ,readr
  ,tidyr
  ,gp2 = ggplot2[ggplot, aes]
  ,gtsummary
)


# globals -----------------------------------------------------------------



# load data ---------------------------------------------------------------

ds0 <- readr$read_rds(here("ML","data-unshared","ds_final.RDS"))

# data manipulation -------------------------------------------------------

#here I am adding a column to determine if the Free T4 Value is diagnostic or not
# using the FT4 Referance range low as the cut off (0.93)


ds1 <- ds0 %>%
  dplyr$mutate(dplyr$across(
    ft4_dia
    , ~factor(., levels = c("Hypo", "Non-Hypo","Hyper", "Non-Hyper")
              )
    )
  )

ds_recode <- ds1 %>%
  dplyr$mutate(
    dplyr$across(
      gender
      ,~dplyr$recode(.,"M" = 1, "F" = 2)
    )
    ,dplyr$across(
      ft4_dia
      ,~dplyr$recode(.
                     ,"Hypo"       = 1
                     ,"Non-Hypo"   = 2
                     ,"Hyper"      = 3
                     ,"Non-Hyper"  = 4
      )
    )
  )


# basic visualization -----------------------------------------------------

#summary Table

summary_tbl <- ds1 %>%
  dplyr$select(-subject_id, -charttime) %>%
  gtsummary$tbl_summary(
    by = ft4_dia
    ,missing = "no"
    ,type = gtsummary$all_continuous() ~ "continuous2"
    ,label = list(
      gender ~ "Gender"
      ,anchor_age ~ "Age"
      )
    ,statistic = gtsummary$all_continuous() ~ c(
      "{N_miss}"
      ,"{median} ({p25}, {p75})"
      ,"{min}, {max}"
      )
    ) %>%
  gtsummary$bold_labels() %>%
  gtsummary$add_stat_label(
    label = gtsummary$all_continuous() ~ c("Missing", "Median (IQR)", "Range")
  ) %>%
  gtsummary$modify_header(label = "**Variable**") %>%
  gtsummary$modify_spanning_header(gtsummary$all_stat_cols() ~ "**Free T4 Diagnostic**")

# summary_tbl



# correlation plot
corr_plot <- cor(
  ds1 %>% dplyr$select(-gender,-ft4_dia, -subject_id, -charttime)
  ,use = "complete.obs"
  ) %>%
  corrplot::corrplot(method = "number", type = "lower", tl.col = "black", tl.srt = 45
                     ,col = corrplot::COL1("Greys"))

# pick color blind friendly pallete


#code for saving corr plot
devEMF::emf(here("figures","corrplot.emf"))
corrplot::corrplot(ds_corr, method = "number")
dev.off()


#quick recode of gender, will still do recoding during feature engineering
g1 <- ds1 %>%
  dplyr$select(-gender,-ft4_dia, -subject_id, -charttime) %>%
  tidyr$pivot_longer(cols = dplyr$everything()) %>%
  ggplot(aes(x = value)) +
  gp2$geom_histogram(na.rm = TRUE) +
  gp2$facet_wrap(~name, scales = "free") +
  gp2$theme_bw() +
  gp2$labs(
    x = NULL
    ,y = NULL
  )

# g1

gp2$ggsave(
  here("figures","distrubution_histo.emf")
  ,width  = 7
  ,height = 7
  ,dpi    = 300
  ,device = devEMF::emf
)

# this takes a bit to load.  No discernible patterns in the data
g2 <- ds_recode %>%
  dplyr$select(-gender, -subject_id, -charttime) %>%
  dplyr$mutate(dplyr$across(-ft4_dia, log)) %>%
  tidyr$pivot_longer(cols = !ft4_dia) %>%
  ggplot(aes(x = factor(ft4_dia), y = value, fill = factor(ft4_dia))) +
  gp2$stat_boxplot(geom = "errorbar", na.rm = TRUE) +
  gp2$geom_boxplot(na.rm = TRUE, outlier.shape = NA) +
  gp2$facet_wrap(~name, scales = "free") +
  gp2$theme_bw() +
  gp2$scale_fill_brewer(
    palette = "Greys"
    ,labels = c("1 - Hypo","2 - Non-Hypo","3 - Hyper","4 - Non-Hyper")
  ) +
  gp2$labs(
    x = NULL
    ,y = NULL
    ,fill = "Lab Diagnosis"
    ,caption = "Note. All values log transformed"
  ) +
  gp2$theme(
    plot.caption = gp2$element_text(hjust = 0)
  )

# g2
gp2$ggsave(
  here("figures","boxplot.emf")
  ,width  = 7
  ,height = 7
  ,dpi    = 300
  ,device = devEMF::emf
  )


# save-data ---------------------------------------------------------------

ds1 %>% readr$write_rds(here("ML","data-unshared","model_data.RDS"))





