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
  ,50995	#Thyroxine (T4), FreE
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
  dplyr$filter(!is.na(`50993`)) %>%
  dplyr$collect()

ds <- ds %>% dplyr$collect()

# close database ----------------------------------------------------------

dbDisconnect(db)
