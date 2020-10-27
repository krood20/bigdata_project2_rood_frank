library(tidyverse)

file_to_table <- function(path, colnames, header = FALSE) {
  stopifnot(file.exists(path))
  table <- read.table(
    path, 
    header=header, 
    sep = ",", 
    stringsAsFactors = TRUE,
    strip.white = TRUE
  )
  if(!(header || missing(colnames))){
    colnames(table) <- colnames
  }
  table
}

attr(file_to_table, 'comment') <- 'Converts census data to table using optional column names'
attr(file_to_table, 'help') <- 'Converts strings to factors, strips whitespace'

create_census <- function(path){
  header <- c('age', 'workclass', 'fnlwgt', 'education', 'education_num',
              'marital_status', 'occupation', 'relationship', 'race', 'sex',
              'capital_gain', 'capital_loss', 'hours_per_week', 'native_cournty',
              'income')
  census <- file_to_table(path, header)
  census <- subset(census, select=-fnlwgt)
  as.data.frame(census)
}

attr(create_census, 'comment') <- 'Returns census data frame from path, removes fnlwgt column'
attr(create_census, 'help') <- 'Designed to work with census data file'

correlate_census <- function(frame, num_to_eval){
  # Create frame of top 5 attributes that influence 'income'
  factors <- subset(frame, select=-income)
  cor <- cor(factors, frame['income'])
  cor.frame <- as.data.frame(cor)
  cor.frame <- cor.frame %>% arrange(desc(income))

  top_cor <- row.names(cor.frame)[1:num_to_eval]
  print("Top correlated")
  print (top_cor)
  bottom_cor <- rev(row.names(cor.frame))[1:num_to_eval]
  print('Bottom correlated')
  print(bottom_cor)

  top_cor <- append(top_cor, 'income')
  smallds <- subset(frame, select=top_cor)
  ggcorr(smallds, label = TRUE)
  ggpairs(
    smallds,
    lower = list(continuous = wrap("points", alpha = 0.3, size=0.1))
  )
}

attr(correlate_census, 'comment') <- 'Calculates top N correlated attributes against income, displays using ggcor, and ggplot'
attr(correlate_census, 'help') <- 'Works with numerical census (factor as numeric), to determine N most correlated pairs'

plot_census_characteristics <- function(census){
  # Sex vs income
  ggplot(census) + aes(x = age, group=income, fill=income) + 
    geom_histogram(binwidth = 1, color = 'black')
  
  # Hours/w vs income
  ggplot(census) + aes(x = hours_per_week, group=income, fill=income) + 
    geom_histogram(binwidth = 10, color = 'black')
  
  # Capital gain vs income
  ggplot(census) + aes(x=capital_gain, group=income, fill=income) +
    geom_histogram(bins=10, color='black')
}

attr(plot_census_characteristics, 'comment') <- 'Plots charecteristics of census data'
attr(plot_census_characteristics, 'help') <- 'Based on factors influencing income, display histograms given census'

get_accuracy <- function(predictions, labels) {
   matching <- Map(function(income, labels) { income %in% labels }, predictions, labels)
   tf <- unlist(matching, use.names=FALSE)
   return (sum(tf)/length(tf))
}

