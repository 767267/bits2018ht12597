# logistic regression for prediction

library(caret)
library(tidyverse)  # data manipulation
source("read_diab_file.r")
source("get_filtered_data.r")

raw = 1

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

str(dat)

if(raw == 1)
{
  dat <- read_diab_file();
}
if(raw == 0)
{
  dat <- get_filtered_data();
}

ind <- sample(2, nrow(dat), replace = TRUE, prob = c(0.7, 0.3))

train_data <- dat[ind == 1,]
test_data <- dat[ind == 2,]

#nrow(train_data)
#nrow(test_data)

str(train_data)

set.seed(123)


logitMod <- glm(Outcome ~ ., data=train_data, family=binomial(link="logit"))

predicted <- plogis(predict(logitMod, test_data))  # predicted scores
# or
#predicted <- predict(logitMod, test_data, type="class")  # predicted scores

pred_new <- sapply(predicted, function(d) round(d, digits = 0))

confMatrix <- table(test_data$Outcome,pred_new)

accuracy <- sum(diag(confMatrix))/sum(confMatrix)
cat("accuracy:",accuracy,"%")

