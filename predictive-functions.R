library(GGally)
library(lsr)

file_to_table <- function(path, colnames, header = FALSE) {
  stopifnot(file.exists(path))
  table <- read.table(path, header=header, sep = ",", stringsAsFactors = TRUE)
  if(!(header || missing(colnames))){
    colnames(table) <- colnames
  }
  table
}

clean_census <- function(table){
  table <- subset(table, select=-c(fnlwgt, education_num))
  table <- lapply(table, function(x) {
    if(is.factor(x)) as.numeric(x) else x
  })
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
  top_cor <- row.names(cor.frame)[1:num_to_eval]
  top_cor <- append(top_cor, 'income')
  smallds <- subset(frame, select=top_cor)
  ggcorr(smallds, label = TRUE, palette = "RdGy")
}

get_accuracy <- function(predictions, labels) {
   matching <- Map(function(income, labels) { income %in% labels }, predictions, labels)
   tf <- unlist(matching, use.names=FALSE)
   return (sum(tf)/length(tf))
}

