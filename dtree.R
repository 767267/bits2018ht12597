#reference - https://www.guru99.com/r-decision-trees.html

library(dplyr)
library(rpart)
library(rpart.plot)

source("read_diab_file.r")
source("get_filtered_data.r")

create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}


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

raw = 1

if(raw == 1)
{
  dat <- read_diab_file();
}
if(raw == 0)
{
  dat <- get_filtered_data();
}
filter1 <- c(5,8,6,9)   # Insulin, Age, BMI > had good correlation with Glucose     - 0.71


dim(dat)


dat <- dat[,filter1]
# Drop variables
clean_dat <- na.omit(dat)
#glimpse(clean_dat)

set.seed(222)

shuffle_index <- sample(1:nrow(clean_dat))

data_train <- create_train_test(clean_dat, 0.8, train = TRUE)
data_test <- create_train_test(clean_dat, 0.8, train = FALSE)
dim(data_train)
dim(data_test)
prop.table(table(data_train$Outcome))

dtree <- rpart(Outcome~., data = data_train, method = 'class' , parms = list(split = "information")) #, parms = list(split = "information")
rpart.plot(dtree, extra = 106)

#prediction
dtree_pred = predict(dtree,data_test,type="class")

confMatrix <- table(data_test$Outcome,dtree_pred)

accuracy <- sum(diag(confMatrix))/sum(confMatrix)
cat("accuracy:",accuracy,"%")
