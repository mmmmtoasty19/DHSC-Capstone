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
)

