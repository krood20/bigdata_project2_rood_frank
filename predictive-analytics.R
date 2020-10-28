library(here)

source("predictive-functions.R")

path <- here("data", "adult.test")
census.factors <- create_census(path)

# Mutate factors into numerical
census.numerical <- lapply(census.factors, function(x) {
  if(is.factor(x)) as.numeric(x) else x
})
census.numerical <- as.data.frame(census.numerical)

correlate_census(census.numerical, 5)
plot_census_characteristics(census.factors)

plot_relatavity(census.factors, 'education_num', 'Years of Education')

breaks <- c(0,20,40,60,80,100)
tags <- c('0-20', '20-40', '40-60', '60-80', '80-800')
group_tags <- cut(
  census.factors$hours_per_week, 
  breaks=breaks,
  labels=tags
)
census.factors$hours_tag <- paste(group_tags)
plot_relatavity(census.factors, 'hours_tag', 'Hours Per Week')

