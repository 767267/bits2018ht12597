# Learning & Evaluation
# https://machinelearningmastery.com/machine-learning-in-r-step-by-step/
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

library(caret)  # data manipulation

source("read_diab_file.r")

dataset = read_diab_file()

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Outcome, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]

# dimensions of dataset
dim(dataset)

# split input and output
x <- dataset[,1:8]
y <- dataset[,9]

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# a) linear algorithms
set.seed(7)
# fit.lda <- train(Outcome~., data=dataset, method="lda", metric=metric, trControl=control)
# # b) nonlinear algorithms
# # CART
# set.seed(7)
# fit.cart <- train(Outcome~., data=dataset, method="rpart", metric=metric, trControl=control)
# # kNN
# set.seed(7)
# fit.knn <- train(Outcome~., data=dataset, method="knn", metric=metric, trControl=control)
# # c) advanced algorithms
# # SVM
# set.seed(7)
# fit.svm <- train(Outcome~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# # Random Forest
# set.seed(7)
# fit.rf <- train(Outcome~., data=dataset, method="rf", metric=metric, trControl=control)

ctrl    <- trainControl(method = "repeatedcv", 
                        number = 4, 
                        savePredictions = TRUE,
                        verboseIter = T,
                        returnResamp = "all")

mod_fit <- train(Outcome~., 
                 data=dataset, 
                 method = "glm", 
                 family="polynomial", 
                 trControl = ctrl)
