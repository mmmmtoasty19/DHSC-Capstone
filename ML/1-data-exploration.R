

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
)


# globals -----------------------------------------------------------------

test_list_names <- c(
   "BUN"   = "51006"
   ,"CA"   = "50893"
   ,"CO2"  = "50882"
   ,"CL"   = "50902"
   ,"CREA" = "50912"
   ,"GLU"  = "50931"
   ,"K"    = "50971"
   ,"NA"   = "50983"
   ,"TSH"  = "50993"
   ,"FT4"  = "50995"
   ,"RBC"  = "51279"
   ,"WBC"  = "51300"
   ,"HCT"  = "51221"
   ,"HGB"  = "51222"
   ,"PLT"  = "51265"
)



# load data ---------------------------------------------------------------

ds_high_tsh_raw <-   readr$read_rds(
    here("ML","data-unshared","ds_high_tsh.RDS")
  )



# data manipulation -------------------------------------------------------

#here I am adding a column to determine if the Free T4 Value is diagnostic or not
# using the FT4 Referance range low as the cut off (0.93)


ds_high_tsh <- ds_high_tsh_raw %>%
  dplyr$mutate(ft4_dia = dplyr$if_else(`50995` < 0.93, 1, 0)) %>%
  #can rename with a vector using either of these
  # dplyr$rename_with(~names(test_list_names), dplyr$all_of(test_list_names))
  dplyr$rename(!!!test_list_names)



# basic visualization -----------------------------------------------------

g_count <- dplyr$as_tibble(colSums(is.na(ds_high_tsh)), rownames = NA ) %>%
  tibble::rownames_to_column() %>%
  ggplot(aes(x = rowname, y = value)) +
  gp2$geom_col() +
  gp2$theme(
    axis.text.x = gp2$element_text(angle = 90)
  )

g_count

ds_corr <- cor(ds_high_tsh %>% dplyr$select(-subject_id, - charttime)
               %>% dplyr$mutate(dplyr$across(gender, ~dplyr$recode(.,M = 1, F = 2)))
               ,use = "complete.obs")


#code for saving corr plot
png(here("figures","corrplot_high.png"), type = 'cairo')

corrplot::corrplot(ds_corr, method = "number")

dev.off()






#quick recode of gender, will still do recoding during feature engineering
g1 <- ds_high_tsh %>%
  dplyr$select(-subject_id, - charttime) %>%
  dplyr$mutate(dplyr$across(gender, ~dplyr$recode(.,M = 1, F = 2))) %>%
  tidyr$pivot_longer(cols = dplyr$everything()) %>%
  ggplot(aes(x = value)) +
  gp2$geom_histogram() +
  gp2$facet_wrap(~name, scales = "free")
g1




g2 <- ds_high_tsh %>%
  dplyr$select(-gender, -subject_id, - charttime) %>%
  tidyr$pivot_longer(cols = !ft4_dia) %>%
  ggplot(aes(x = factor(ft4_dia), y = value, fill = factor(ft4_dia))) +
  gp2$geom_boxplot(outlier.shape = NA, na.rm = TRUE) +
  gp2$geom_jitter(size=.7, width=.1, alpha=.5, na.rm = TRUE) +
  gp2$facet_wrap(~name, scales = "free")

g2