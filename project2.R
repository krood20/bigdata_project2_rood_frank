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
subset_cols <- c("age", "race", "sex", "education", "occupation")
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

#TODO: Build a table with your results succinctly displayed. For some reason accuracy is always 0
accuracy

#Document your results in your report in separate sections. 
#Show screen shots of plots of the clusters.
#plot stuff for kmeans clustering

#4. TODO: Prediction

#Use adult.test as your test set.

#Try the lm(…) method to get linear fits for the data. 
#This will not work on all attributes, so you must determine which ones it will work on. 

#Try clusters of 3,5,7,9 --> generate labels using kmeans.
#Do a cross-tabulation of the results to see how good you did.


#Calculate the accuracy, false positive, etc. and display in a table.

#Try different parameter values and combinations of attributes.
#Use predict(…) to predict the values for the test set after training with the training set.

#Try the glm(…)method to get linear fits for the data. This will not work on all attributes, so you must determine which ones it will work on. 

#Try the lm(…) method to get linear fits for the data. 
#This will not work on all attributes, so you must determine which ones it will work on. 

#What differences to do you see between kmeans and kNN?
#What differences do you see between lm(…) and glm(….)?
  
