rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run.
cat("\014") # Clear the console


# load packages -----------------------------------------------------------

box::use(
  magrittr[`%>%`]
  ,RSQLite
  ,DBI[dbConnect,dbDisconnect]
  ,here[here]
)

# globals -----------------------------------------------------------------

db <- dbConnect(
  RSQLite$SQLite()
  ,here("ML","data-unshared","mimicDB.sqlite")
)









# close database ----------------------------------------------------------

dbDisconnect(db)
