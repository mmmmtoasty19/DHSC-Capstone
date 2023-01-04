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
  ,DBI
  ,here[here]
)



# globals -----------------------------------------------------------------




# create database ---------------------------------------------------------

mimicDB <- DBI$dbConnect(
  RSQLite$SQLite()
  ,here("ML","data-unshared","mimicDB.sqlite")
  )






# close database ----------------------------------------------------------

DBI$dbDisconnect(mimicDB)

