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
  ,GGally
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
  dplyr$select(-subject_id, -charttime, -FT4) %>%
  gtsummary$tbl_summary(
    by = ft4_dia
    ,missing = "no"
    ,type = gtsummary$all_continuous() ~ "continuous"
    ,label = list(
      gender ~ "Gender"
      ,anchor_age ~ "Age"
    )
    ,statistic = gtsummary$all_continuous() ~ c("{median} ({p25}, {p75})")
  ) %>%
  # gtsummary$bold_labels() %>%
  gtsummary$add_n(statistic = "{p_miss}", col_label = "**% Missing**") %>%
  gtsummary$modify_header(label = "**Variable**") %>%
  gtsummary$modify_spanning_header(gtsummary$all_stat_cols() ~ "**Free T4 Outcome**")


# summary_tbl


# corr-plot ---------------------------------------------------------------

corr_plot <- ds1 %>%
  dplyr$select(-gender,-ft4_dia, -subject_id, -charttime, -FT4) %>%
  dplyr$rename(Age = anchor_age) %>%
  GGally$ggcorr(nbreaks = 5, palette = "Greys"
                ,label = TRUE, label_size = 3, label_color = "white"
                  ,label_round = 2
                ,hjust = 0.75
                ,layout.exp = 1)

# corr_plot

gp2$ggsave(
  here("figures","corr_plot.emf")
  ,width  = 7
  ,height = 7
  ,dpi    = 300
  ,device = devEMF::emf
)
gp2$ggsave(
  here("figures","corr_plot.png")
  ,width  = 7
  ,height = 7
  ,dpi    = 300
)


#quick recode of gender, will still do recoding during feature engineering
g1 <- ds1 %>%
  dplyr$select(-gender,-ft4_dia, -subject_id, -charttime, -FT4) %>%
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
gp2$ggsave(
  here("figures","distrubution_histo.png")
  ,width  = 7
  ,height = 7
  ,dpi    = 300
)

# this takes a bit to load.  No discernible patterns in the data
g2 <- ds_recode %>%
  dplyr$select(-gender, -subject_id, -charttime, -FT4) %>%
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
gp2$ggsave(
  here("figures","boxplot.png")
  ,width  = 7
  ,height = 7
  ,dpi    = 300
)


# save-data ---------------------------------------------------------------

ds1 %>% readr$write_rds(here("ML","data-unshared","model_data.RDS"))





