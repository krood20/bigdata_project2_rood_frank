library(here)
library(psych)

source("predictive-functions.R")

path.real <- here("data", "adult.data")
census.factors <- create_census(path.real)

# Mutate factors into numerical
census.numerical <- lapply(census.factors, function(x) {
  if(is.factor(x)) as.numeric(x) else x
})
census.numerical <- as.data.frame(census.numerical)

# Commenting because its documented, and slow
# correlate_census(census.numerical, 5)

plot_relatavity(census.factors, 'education_num', 'Years of Education')

breaks <- c(0,20,40,60,80,100)
tags <- c('<20', '20-40', '40-60', '60-80', '80-100')
plot_bins(census.factors, 'hours_per_week', breaks, tags, 'Hours Per Week')

breaks <- c(0, 1, 25000, 10000000)
tags <- c('0', '1-25K', '25K+')
plot_bins(census.factors, 'capital_gain', breaks, tags, 'Capital Gain')

plot_distribution(census.factors, 'age', 'Age')

census.means <- subset(census.numerical, select=-income)
census.means <- as.data.frame(census.means)

# KMeans
for(k in 2:10){
  means <- kmeans(census.means, k)
  print(table(data.frame(census.factors$income, means$cluster)))
  acc <- 100*(means$betweenss/means$totss)
  message(sprintf('KMeans: k=%d accuracy=%f%%', k, acc))
}

path.test <- here("data", "adult.test")
census.test.factors <- create_census(path.test)
census.test.numerical <- lapply(census.test.factors, function(x) {
  if(is.factor(x)) as.numeric(x) else x
})
census.test.numerical <- as.data.frame(census.test.numerical)
census.test.means <- subset(census.test.numerical, select=-income)
census.test.means <- as.data.frame(census.test.means)

#KNN
for(k in 2:10){
  knn <- knn(train=census.means, test=census.test.means, cl=census.numerical$income, k=k)
  print(table(census.test.factors$income, knn))
  acc <- 100 * sum(census.test.numerical$income == data.frame(knn))/length(census.test.numerical$income)
  message(sprintf('KNN: K=%d, accuracy=%f%%', k, acc))
}

#IClust
clust <- iclust(census.means, nclusters=2)
for(k in 2:10){
  clust <- iclust(census.means, nclusters=k, plot = FALSE)
  message(sprintf('Iclust: Pattern fit %f%%', clust$p.fit$patternfit * 100))
  message(sprintf('Iclust: Cluster fit %f%%', clust$p.fit$clusterfit * 100))
}
