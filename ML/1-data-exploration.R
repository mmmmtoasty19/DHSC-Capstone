rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run.
cat("\014") # Clear the console


# load packages -----------------------------------------------------------

box::use(
  magrittr[`%>%`]
  ,here[here]
  ,dplyr
  ,readr
  ,tidyr
  ,ggplot2
)


# globals -----------------------------------------------------------------





# load data ---------------------------------------------------------------

ds_high_tsh <-   readr$read_rds(
    here("ML","data-unshared","ds_high_tsh.RDS")
  )



# data manipulation -------------------------------------------------------

#here I am adding a column to determine if the Free T4 Value is diagnostic or not
# using the FT4 Referance range low as the cut off (0.93)


ds_high_tsh <- ds_high_tsh %>%
  dplyr$mutate(ft4_dia = dplyr$if_else(`50995` < 0.93, 1, 0))



# basic visualization -----------------------------------------------------

test <- dplyr$as_tibble(colSums(is.na(ds_high_tsh)), rownames = NA ) %>%
  tibble::rownames_to_column()



g1 <- ds_high_tsh %>%
  dplyr$select(-subject_id, - charttime) %>%
  dplyr$mutate(dplyr$across(gender, ~dplyr$recode(.,M = 1, F = 2))) %>%
  tidyr$pivot_longer(cols = dplyr$everything())

