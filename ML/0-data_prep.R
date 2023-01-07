rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run.
cat("\014") # Clear the console


# load packages -----------------------------------------------------------

box::use(
  magrittr[`%>%`]
  ,RSQLite
  ,DBI[dbConnect,dbDisconnect]
  ,here[here]
  ,dplyr
  ,dbplyr
  ,tidyr
  ,readr
)

# globals -----------------------------------------------------------------

db <- dbConnect(
  RSQLite$SQLite()
  ,here("ML","data-unshared","mimicDB.sqlite")
)

#item list shows two different numbers for a few tests, second set of items do not have
# any results that are on the same samples as TSH and Free T4
test_list_cmp <- c(
  50862 #Albumin
  ,50863	#Alkaline Phosphatase
  ,50861	#Alanine Aminotransferase (ALT)
  ,50878	#Asparate Aminotransferase (AST)
  ,51006	#Urea Nitrogen
  ,50893	#Calcium, Total
  ,50882	#Bicarbonate
  ,50902	#Chloride
  ,50912	#Creatinine
  ,50931	#Glucose
  ,50971	#Potassium
  ,50983	#Sodium
  ,50885	#Bilirubin, Total
  ,50976	#Protein, Total
  ,50993	#Thyroid Stimulating Hormone
  ,50995	#Thyroxine (T4), Free
)

test_list_bmp <- c(
  51006	#Urea Nitrogen
  ,50893	#Calcium, Total
  ,50882	#Bicarbonate
  ,50902	#Chloride
  ,50912	#Creatinine
  ,50931	#Glucose
  ,50971	#Potassium
  ,50983	#Sodium
  ,50993	#Thyroid Stimulating Hormone
  ,50995	#Thyroxine (T4), Free
)

# TSH Ref Range from File 0.27 - 4.2 uIU/mL
# Free T4 Ref Range from File 0.93 - 1.7 ng/dL

# load data ---------------------------------------------------------------

# load patients first to add to lab values
patients <- dplyr$tbl(db, "patients") %>%
  dplyr$select(-anchor_year, -anchor_year_group, -dod) %>%
  dplyr$collect()

# most likely will not use this as there are not as many complete rows.  However
# gathering it just in case.
# first is using specimen id, usable data set is using chart time as it appears
# LIS uses different id's for groups of tests
#
# ds_cmp <- dplyr$tbl(db, "labevents") %>%
#   dplyr$filter(itemid %in% test_list_cmp) %>%
#   dplyr$select(-charttime,-storetime) %>%
#   tidyr$pivot_wider(
#     id_cols = c(subject_id,specimen_id)
#     ,names_from = itemid
#     ,values_from = valuenum
#     ) %>%
#   dplyr$filter(!is.na(`50993`) & !is.na(`50995`)) %>%
#   dplyr$filter(dplyr$across(where(is.numeric), ~!is.na(.x))) %>%
#   dplyr$collect()

ds_cmp <- dplyr$tbl(db, "labevents") %>%
  dplyr$filter(itemid %in% test_list_cmp) %>%
  dplyr$select(-storetime) %>%
  tidyr$pivot_wider(
    id_cols = c(subject_id,charttime)
    ,names_from = itemid
    ,values_from = valuenum
  ) %>%
  dplyr$filter(!is.na(`50993`) & !is.na(`50995`)) %>%
  dplyr$collect()

#this keeps failing if run as part of the above query.  Moving here to keep going
# keeps only rows that have values for all columns
ds_cmp <- patients %>%
  dplyr$left_join(ds_cmp, by = c("subject_id" = "subject_id")) %>%
  dplyr$filter(dplyr$if_all(.fns = ~!is.na(.)))



ds_bmp <- dplyr$tbl(db, "labevents") %>%
  dplyr$filter(itemid %in% test_list_bmp) %>%
  dplyr$select(-storetime) %>%
  tidyr$pivot_wider(
    id_cols = c(subject_id,charttime)
    ,names_from = itemid
    ,values_from = valuenum
  ) %>%
  dplyr$filter(!is.na(`50993`) & !is.na(`50995`)) %>%
  dplyr$collect()

ds_bmp <- patients %>%
  dplyr$left_join(ds_bmp, by = c("subject_id" = "subject_id")) %>%
  dplyr$filter(dplyr$if_all(.fns = ~!is.na(.)))


# save data ---------------------------------------------------------------


ds_high_tsh <- ds_bmp %>%
  dplyr$filter(`50993` > 4.2) %>%
  readr$write_rds(
    here("ML","data-unshared","ds_high_tsh.RDS")
  )

ds_low_tsh <- ds_bmp %>%
  dplyr$filter(`50993` < 0.27) %>%
  readr$write_rds(
    here("ML","data-unshared","ds_low_tsh.RDS")
  )

# close database ----------------------------------------------------------

dbDisconnect(db)
