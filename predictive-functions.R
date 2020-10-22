
file_to_table <- function(path, colnames, header = FALSE) {
  stopifnot(file.exists(path))
  table <- read.table(path, header=header, sep = ",", stringsAsFactors = TRUE)
  if(!(header || missing(colnames))){
    colnames(table) <- colnames
  }
  table
}

label_data <- function(table){
  sapply(table, function(x){
    if(is.factor(x)) factor(x) else x
  })
}