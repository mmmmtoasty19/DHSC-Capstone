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
# 51301 and 51300 looks like test name may have changed
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
  ,51279  #RBC
  ,51301	#White Blood Cells
  ,51300	#WBC Count
  ,51221	#Hematocrit
  ,51222	#Hemoglobin
  ,51265	#Platelet Count
  )

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

# TSH Ref Range from File 0.27 - 4.2 uIU/mL
# Free T4 Ref Range from File 0.93 - 1.7 ng/dL

# load data ---------------------------------------------------------------

# load patients first to add to lab values
patients <- dplyr$tbl(db, "patients") %>%
  dplyr$select(-anchor_year, -anchor_year_group, -dod) %>%
  dplyr$collect()

# usable data set is using chart time as it appears
# LIS uses different id's for groups of tests
# BMP and CBC Results together

ds_bmp <- dplyr$tbl(db, "labevents") %>%
  dplyr$filter(itemid %in% test_list_bmp) %>%
  dplyr$select(-storetime) %>%
  tidyr$pivot_wider(
    id_cols = c(subject_id,charttime)
    ,names_from = itemid
    ,values_from = valuenum
  ) %>%
  dplyr$collect()


ds1 <- ds_bmp %>%
  dplyr$filter(!is.na(`50993`) & !is.na(`50995`)) %>%
  dplyr$left_join(patients, by = c("subject_id" = "subject_id")) %>%
  dplyr$mutate(dplyr$across(`51300`, ~dplyr$if_else(!is.na(.),`51300`,`51301`))) %>%
  dplyr$select(-`51301`, -hadm_id) %>%
  # dplyr$filter(dplyr$if_all(.fns = ~!is.na(.)))
  dplyr$filter(rowSums(is.na(.)) <= 3)  #allows for 3 missing test

ds_final <- ds1 %>%
  dplyr$mutate(
    ft4_dia = dplyr$case_when(
      `50993` > 4.2 & `50995` < 0.93 ~ "Hypo"
      ,`50993` > 4.2 & `50995` > 0.93 ~ "Non-Hypo"
      ,`50993` < 0.27 & `50995` > 1.7  ~ "Hyper"
      ,`50993` < 0.27 & `50995` < 1.7  ~ "Non-Hyper"
      ,TRUE ~ "Normal TSH"
    )
  ) %>%
  dplyr$rename(!!!test_list_names) %>%
  dplyr$relocate(gender, anchor_age)

# save data ---------------------------------------------------------------

ds_final %>% readr$write_rds(here("ML","data-unshared","ds_final.RDS"))


# close database ----------------------------------------------------------

dbDisconnect(db)
