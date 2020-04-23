# random forest
library(randomForest)
library(caret)
library(tidyverse)  # data manipulation

source("read_diab_file.r")
source("get_filtered_data.r")


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


dat <- read_diab_file();

ind <- sample(2, nrow(dat), replace = TRUE, prob = c(0.7, 0.3)) # 70/30 training and test

train_data <- dat[ind == 1,]
test_data <- dat[ind == 2,]

nrow(train_data)
nrow(test_data)

#idea on structure 
#str(train_data)


set.seed(234)

rand <- randomForest(Outcome~., data=train_data) #, method = 'class' , parms = list(split = "information")

#attributes(rand)

pred1 <- predict(rand, test_data[-9],type="class" )

pred_new <- sapply(pred1, function(d) round(d, digits = 0))

confMatrix <- table(test_data$Outcome,pred_new)
accuracy <- sum(diag(confMatrix))/sum(confMatrix)
cat("accuracy:",accuracy,"%")

plot(rand)


varImpPlot(rand)
    
# repeating with efficient no of trees observed from previous stage

rand <- randomForest(Outcome~., data=train_data, ntree=100) 

pred1 <- predict(rand, test_data[-9],type="class" )

pred_new <- sapply(pred1, function(d) round(d, digits = 0))

confMatrix <- table(test_data$Outcome,pred_new)
accuracy <- sum(diag(confMatrix))/sum(confMatrix)
cat("accuracy:",accuracy,"%")

plot(rand)


varImpPlot(rand)

