# kemans - https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/kmeans

# columns 
#    1 Pregnancies, 
#    2 Glucose, 
#    3 BloodPressure, 
#    4 SkinThickness, 
#    5 Insulin,  
#    6 BMI, 
#    7 DiabetesPedigreeFunction, 
#    8 Age, 
#    9 Outcome

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(caret)  # data manipulation
library(clue)
source("read_diab_file.r")
source("get_filtered_data.r")

predict.kmeans <- function(object,
                           newdata,
                           method = c("centers", "classes")) {
  method <- match.arg(method)
  
  centers <- object$centers
  ss_by_center <- apply(centers, 1, function(x) {
    colSums((t(newdata) - x) ^ 2)
  })
  best_clusters <- apply(ss_by_center, 1, which.min)
  
  if (method == "centers") {
    centers[best_clusters, ]
  } else {
    best_clusters
  }
}

filter1 <- c(5,8,6)   # Insulin, Age, BMI > had good correlation with Glucose     - 0.71
filter2 <- c(5,8,6,3) # Insulin, Age, BMI, BP > had good correlation with Glucose - 0.66
filter3 <- c(5,8,6,4) # Insulin, Age, BMI, Skin Thickness > had good correlation with Glucose - 0.68
filter4 <- c(5,8,6,2) # Insulin, Age, BMI, Glucose

filter5 <- c(5,8,6,7)   # Insulin, Age, BMI, DPF

no_of_clusters = 3

dat = read_diab_file()


# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dat$Outcome, p=0.80, list=FALSE)
# select 20% of the data for validation
test_data <- dat[-validation_index,]
# use the remaining 80% of data to training and testing the models
train_data <- dat[validation_index,]
dim(test_data)
dim(train_data)

kmeans_results <- kmeans(train_data, 3)

fitted(kmeans_results)

custom_predict <- predict.kmeans(kmeans_results, test_data, method = "classes")



clue_predict <- cl_predict(kmeans_results, test_data, type = "memberships")

# find(clue_predict[1,], 1)
# 
# 
# for(i in 1:nrow(test_data))
# {
#    t <- clue_predict[i,]
# }