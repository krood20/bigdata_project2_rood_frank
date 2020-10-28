library(tidyverse)
library(lazyeval)

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
  print(cor.frame)

  top_cor <- row.names(cor.frame)[1:num_to_eval]
  print("Top correlated")
  print (top_cor)
  bottom_cor <- rev(row.names(cor.frame))[1:num_to_eval]
  print('Bottom correlated')
  print(bottom_cor)

  top_cor <- append(top_cor, 'income')
  smallds <- subset(frame, select=top_cor)
  cor.chart <- ggcorr(smallds, label = TRUE)
  print(cor.chart)
  cor.pairs <- ggpairs(
    smallds,
    lower = list(continuous = wrap("points", alpha = 0.3, size=0.1))
  )
  print(cor.pairs)
}

attr(correlate_census, 'comment') <- 'Calculates top N correlated attributes against income, displays using ggcor, and ggplot'
attr(correlate_census, 'help') <- 'Works with numerical census (factor as numeric), to determine N most correlated pairs'

plot_census_characteristics <- function(census){
  # Sex vs income
  sex_income <- ggplot(census) + aes(x = age, group=income, fill=income) + 
    geom_histogram(binwidth = 1, color = 'black')
  print(sex_income)
  
  # Hours/w vs income
  hours_income <- ggplot(census) + aes(x = hours_per_week, group=income, fill=income) + 
    geom_histogram(binwidth = 10, color = 'black')
  print(hours_income)
  
  # Capital gain vs income
  ggplot(census) + aes(x=capital_gain, y=..density.., group=income, fill=income) +
   geom_histogram(bins=10, color='black') +
   scale_y_continuous(labels = scales::percent_format())
}

attr(plot_census_characteristics, 'comment') <- 'Plots charecteristics of census data'
attr(plot_census_characteristics, 'help') <- 'Based on factors influencing income, display histograms given census'

plot_relatavity <- function(census.factors, column, xlab){
  income_ed <- data.frame(table(census.factors$income, census.factors[[column]]))
  colnames(income_ed) <- c('income', column, 'count')
  income_ed <- income_ed %>% 
    group_by_(column) %>%  
    mutate(percent = (count/sum(count)))
  
  ggplot(income_ed) +
    aes_string(x = column, y = 'percent', group = 'income', fill = 'income') +
    geom_bar(stat="identity", position="dodge", color = 'black') +
    labs(x = xlab, y = 'Percent', fill = "Income") + 
    scale_y_continuous(labels = scales::percent_format())
}

plot_bins <- function(census.factors, column, breaks, tags, title){
  group_tags <- cut(
    census.factors[[column]],
    breaks=breaks,
    labels=tags,
    include.lowest = TRUE,
    right =TRUE
  )
  census.factors[[column]] <- paste(group_tags)
  plot_relatavity(census.factors, column, title)
}

get_accuracy <- function(predictions, labels) {
   matching <- Map(function(income, labels) { income %in% labels }, predictions, labels)
   tf <- unlist(matching, use.names=FALSE)
   return (sum(tf)/length(tf))
}

