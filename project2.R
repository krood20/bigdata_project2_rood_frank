#For adult.data, remove the classification, e.g., the last column in the data:
#Adult.data will be your training set for item 4 below.
library(here)
path <- here("data", "adult.data")
data <- read.table(path)
data

classification <- data[,ncol(data)]
classification

#For adult.data, remove the classification, e.g., the last column in the data:
#Adult.data will be your training set for item 4 below.
data[15] <- NULL #remove last column
data

#plot the data using pairwise plotting to get a sense of the relationships between the attributes.
data_numeric <- data.matrix(table)
data_numeric[] <- lapply(data, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(data_numeric, class)
data_numeric

pairs(data_numeric, pch = 19)


#plotting the data using several plotting functions to see what it looks like
#Use pairs (e.g., 2D plots) or 3 variables (3D plots) based on the packages. 

#Which pairs of attributes seem to be correlated? How are they correlated?
  


#2. Prepare the Data
#Investigate some of the statistics of the data set

#You may have to subset the data
#To subset, pick the most correlated attributes to use – they may all be relevant 
#document your rationale for eliminating some attributes.


#translate alphanumeric (e.g., character) values into numeric values.
#Create a mapping for each field of values to integers.
#Make sure you put these mappings into your report.



#3. Clustering
#Eliminate <=50K and >50K column from the data set.
#Apply three clustering techniques to the subsetted data: KMeans, kNN, and iClust.
#You should analyze the data for clusters from k =2 to 10.

#Build a table with your results succinctly displayed.

#Document your results in your report in separate sections. 
#Show screen shots of plots of the clusters.

#4. Prediction

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
  
