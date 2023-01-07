# The follow script rebuilds a local copy of the MIMIC-IV database
# for the purpose of this project only the patients, labitems, and labevents
# tables are included, and the csv's need to be in the data-unshared folder
# this script only needs to be run once on any computer performing analysis

rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run.
cat("\014") # Clear the console


# load packages -----------------------------------------------------------

box::use(
  magrittr[`%>%`]
  ,RSQLite
  ,DBI[dbConnect,dbDisconnect,dbWriteTable]
  ,here[here]
  ,readr
)



# globals -----------------------------------------------------------------




# create database ---------------------------------------------------------

mimicDB <- dbConnect(
  RSQLite$SQLite()
  ,here("ML","data-unshared","mimicDB.sqlite")
  )


# create labevents table
readr$read_csv_chunked(
  here("ML","data-unshared", "labevents.csv")
  ,callback = function(chunk,dummy){
    dbWriteTable(mimicDB, "labevents", chunk, append = TRUE)
    }
  ,chunk_size = 10000
  ,col_types = "_d_dd_TT_d______"
  )


# create patient table

readr$read_csv(
  here("ML","data-unshared","patients.csv")
) %>%
  dbWriteTable(mimicDB,"patients", .)

# create labitems table

readr$read_csv(
  here("ML","data-unshared","d_labitems.csv")
) %>%
  dbWriteTable(mimicDB,"labitems", .)


# close database ----------------------------------------------------------

dbDisconnect(mimicDB)

