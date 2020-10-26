library(GGally)
library(lsr)

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

clean_census <- function(table){
  table <- subset(table, select=-c(fnlwgt, education_num))
  as.data.frame(table)
}

create_census <- function(path){
  header <- c('age', 'workclass', 'fnlwgt', 'education', 'education_num',
              'marital_status', 'occupation', 'relationship', 'race', 'sex',
              'capital_gain', 'capital_loss', 'hours_per_week', 'native_cournty',
              'income')
  census <- file_to_table(path, header)
  clean_census(census)
}

correlate_census <- function(frame, num_to_eval){
  # Create frame of top 5 attributes that influence 'income'
  factors <- subset(frame, select=-income)
  cor <- correlate(factors, frame['income'])
  cor.frame <- as.data.frame(cor)
  cor.frame <- cor.frame[
    with(cor.frame, order(income, decreasing =TRUE)), 
  ]
  print(cor.frame['income'])
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

get_accuracy <- function(predictions, labels) {
   matching <- Map(function(income, labels) { income %in% labels }, predictions, labels)
   tf <- unlist(matching, use.names=FALSE)
   return (sum(tf)/length(tf))
}

