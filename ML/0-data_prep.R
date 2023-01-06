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
)

# globals -----------------------------------------------------------------

db <- dbConnect(
  RSQLite$SQLite()
  ,here("ML","data-unshared","mimicDB.sqlite")
)

#item list shows two different numbers for a few tests, second set of items do not have
# any results that are on the same samples as TSH and Free T4
test_list <- c(
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



# load data ---------------------------------------------------------------

ds <- dplyr$tbl(db, "labevents") %>%
  dplyr$filter(itemid %in% test_list) %>%
  dplyr$select(-charttime,-storetime) %>%
  tidyr$pivot_wider(
    id_cols = c(subject_id,specimen_id)
    ,names_from = itemid
    ,values_from = valuenum
    ) %>%
  dplyr$filter(!is.na(`50993`) & !is.na(`50995`)) %>%
  dplyr$collect()


ds %>% dplyr$filter(dplyr$across(where(is.numeric), ~!is.na(.x)))

count <- data.frame(colSums(is.na(ds))) %>% tibble::rownames_to_column()


testds <- readr::read_csv(
  here("ML","data-unshared", "labevents.csv")
  ,col_types = "_d_ddTT_d______"
  ,n_max = 100
)




ds1 <- dplyr$tbl(db, "labevents") %>%
  dplyr$filter(itemid %in% test_list) %>%
  dplyr$select(-storetime) %>%
  tidyr$pivot_wider(
    id_cols = c(subject_id,charttime)
    ,names_from = itemid
    ,values_from = valuenum
  ) %>%
  dplyr$filter(!is.na(`50993`) & !is.na(`50995`)) %>%
  dplyr$collect()

count2 <- data.frame(colSums(is.na(ds1))) %>% tibble::rownames_to_column()

counts <- count %>%
  dplyr$left_join(count2)

# close database ----------------------------------------------------------

dbDisconnect(db)
