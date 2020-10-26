library(here)

source("predictive-functions.R")

path <- here("data", "adult.test")
census <- create_census(path)
census.numerical <- lapply(census, function(x) {
  if(is.factor(x)) as.numeric(x) else x
})
census.numerical <- as.data.frame(census.numerical)

correlate_census(census.numerical, 5)
plot_census_characteristics(census)

