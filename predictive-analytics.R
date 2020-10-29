library(here)

source("predictive-functions.R")

path <- here("data", "adult.test")
census.factors <- create_census(path)

# Mutate factors into numerical
census.numerical <- lapply(census.factors, function(x) {
  if(is.factor(x)) as.numeric(x) else x
})
census.numerical <- as.data.frame(census.numerical)

# Commenting because its documented, and slow
#correlate_census(census.numerical, 5)

plot_census_characteristics(census.factors)

plot_relatavity(census.factors, 'education_num', 'Years of Education')

breaks <- c(0,20,40,60,80,100)
tags <- c('<20', '20-40', '40-60', '60-80', '80-100')
plot_bins(census.factors, 'hours_per_week', breaks, tags, 'Hours Per Week')

breaks <- c(0, 1, 25000, 10000000)
tags <- c('0', '1-25K', '25K+')
plot_bins(census.factors, 'capital_gain', breaks, tags, 'Capital Gain')

plot_distribution(census.factors, 'age', 'Age')
