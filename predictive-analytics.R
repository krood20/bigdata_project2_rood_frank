library(here)

source("predictive-functions.R")

path <- here("data", "adult.test")
census <- create_census(path)
correlate_census(census, 5)

