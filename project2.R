#For adult.data, remove the classification, e.g., the last column in the data:
#Adult.data will be your training set for item 4 below.
library(here)
library(FNN)
library(gmodels)
library(tidyverse)
library(cluster)
library(factoextra)

source("predictive-functions.R")
#path <- here("data", "adult.small")
path <- here("data", "adult.data")

header <- c('age', 'workclass', 'fnlwgt', 'education', 'education_num',
            'marital_status', 'occupation', 'relationship', 'race', 'sex',
            'capital_gain', 'capital_loss', 'hours_per_week', 'native_cournty',
            'income')
data <- file_to_table(path, header)
#get smaller amount
data <- data[1:100,]
#data <- read.table(path)
data

classification <- data[,ncol(data)]
classification

#For adult.data, remove the classification, e.g., the last column in the data:
#Adult.data will be your training set for item 4 below.
income <- data[15]
data[15] <- NULL #remove last column
data
income

#plot the data using pairwise plotting to get a sense of the relationships between the attributes.
#plotting the data using several plotting functions to see what it looks like
#Use pairs (e.g., 2D plots) or 3 variables (3D plots) based on the packages. 
smallds <- subset(data, select=c('age', 'race', 'sex', 'hours_per_week', 'income'))
smallds <- subset(data, select=c('age', 'income'))
ggpairs(smallds)
pairs(smallds)

# *** Which pairs of attributes seem to be correlated? How are they correlated? ***


#2. Prepare the Data
#Investigate some of the statistics of the data set

#You may have to subset the data
#To subset, pick the most correlated attributes to use – they may all be relevant 
#document your rationale for eliminating some attributes.
#SEE SUBSETTING BELOW


#translate alphanumeric (e.g., character) values into numeric values.
#Create a mapping for each field of values to integers.
#Make sure you put these mappings into your 
data_numeric <- data.matrix(data)

pairs(data_numeric, pch = 19)

#3. Clustering
#Eliminate <=50K and >50K column from the data set.
data_numeric

#Apply three clustering techniques to the subsetted data: KMeans, kNN, and iClust.
#You should analyze the data for clusters from k =2 to 10.
# subset_cols <- c("age", "race", "sex", "education", "occupation")
subset_cols <- c("age", "sex", "hours_per_week", "capital_gain", "capital_loss")
small_data_numeric <- data_numeric[,subset_cols]
small_data_numeric

#storage of results
knn_results <- vector("list", 9)
kmeans_results <- vector("list", 9)
accuracy <- vector("list", 9)

#variables we will need for knn
test_train_split_index <- floor(nrow(small_data_numeric) * 0.7)
train <- small_data_numeric[1:test_train_split_index,]
test <- small_data_numeric[(test_train_split_index + 1):nrow(small_data_numeric),]

#this part fails when using the small data
for(i in 2:10) {
  print("Num clusters: " + i)
  kmeans_result <- kmeans(small_data_numeric, centers=i) #need to do others as well
  fviz_cluster(kmeans_result, data=small_data_numeric) #plot results

  #knn
  knn_kmeans <- kmeans(train, centers=i)
  knn_labels <- knn_kmeans$cluster
  knn_result <- knn(train, test, knn_labels, k=i)
  preds <- knn_result

  #Not sure if this is what he wants for tables
  #ct <- CrossTable(income[(test_train_split_index + 1):nrow(income),], preds, chisq=FALSE)

  #add to output arrays
  kmeans_results[[i]] <- kmeans_result
  knn_results[[i]] <- knn_result
  accuracy[[i]] <- get_accuracy(income[(test_train_split_index + 1):nrow(income),], preds)
}

#TODO: Still need to implement iClust

#TODO: Build a table with your results succinctly displayed. For some reason accuracy is always 0
accuracy

#Document your results in your report in separate sections. 
#Show screen shots of plots of the clusters.


#4. TODO: Prediction
#Use adult.test as your test set.
path <- here("data", "adult.data")
test_data <- file_to_table(path, header)

#Try the lm(…) method to get linear fits for the data. 
#This will not work on all attributes, so you must determine which ones it will work on.
# [age~sex, ]
#TODO: Try other combos for both of these
simple.fit = lm(age~hours_per_week, data=test_data)
summary(simple.fit)

#Try the glm(…)method to get linear fits for the data. This will not work on all attributes, so you must determine which ones it will work on.
adv.fit = glm(age~hours_per_week, data=test_data)
summary(adv.fit)



#Try clusters of 3,5,7,9 --> generate labels using kmeans.
#clean data
test_income_data <- test_data[15]
data[15] <- NULL #remove last column
test_data_numeric <- data.matrix(data)

test_train_split_index <- floor(nrow(test_data_numeric) * 0.7)
train <- test_data_numeric[1:test_train_split_index,]
test <- test_data_numeric[(test_train_split_index + 1):nrow(test_data_numeric),]

#storage of results
knn_results <- vector("list", 9)
kmeans_results <- vector("list", 9)
kmeans_plots <- vector("list", 9)
accuracy <- vector("list", 9)

for(i in 3:9) {
  if(i%%2 != 0){
    print("Num clusters: ")
    print(toString(i))
    kmeans_result <- kmeans(test_data_numeric, centers=i) #need to do others as well
    kmeans_plots[[i]] <- fviz_cluster(kmeans_result, data=test_data_numeric) #plot results
    kmeans_plots[[i]]
    
    #knn
    knn_kmeans <- kmeans(train, centers=i)
    knn_labels <- knn_kmeans$cluster
    knn_result <- knn(train, test, knn_labels, k=i)
    preds <- knn_result
    
    #knn
    knn_kmeans <- kmeans(train, centers=i)
    knn_labels <- knn_kmeans$cluster
    knn_result <- knn(train, test, knn_labels, k=i)
    preds <- knn_result
    
    #Do a cross-tabulation of the results to see how good you did.
    ct <- CrossTable(income[(test_train_split_index + 1):nrow(income),], preds, chisq=FALSE)
    
    #add to output arrays
    kmeans_results[[i]] <- kmeans_result
    knn_results[[i]] <- knn_result
   
    #TODO: get accuracy --> not working not sure why
    income_labels <- income[(test_train_split_index + 1):nrow(income),]
    matching <- Map(function(income, labels) { income %in% labels }, preds, income_labels)
    tf <- unlist(matching, use.names=FALSE)
    accuracy[[i]] <- (sum(tf)/length(tf))
  }

}


#display graphs
# library(gridExtra)
# grid.arrange(kmeans_plots[0], kmeans_plots[1], kmeans_plots[2], kmeans_plots[3], nrow = 2)



#Calculate the accuracy, false positive, etc. and display in a table.
#Try different parameter values and combinations of attributes.
  #Use predict(…) to predict the values for the test set after training with the training set.
  #TODO: Implement our own accuracy function? To get all false/true positive/negative values?


#Answer in report:
#What differences to do you see between kmeans and kNN?
#What differences do you see between lm(…) and glm(….)?
  
