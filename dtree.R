#reference - https://www.guru99.com/r-decision-trees.html

library(dplyr)
library(rpart)
library(rpart.plot)
source("D:/BITS/project/read_diab_file.r")

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

dat = read_diab_file();


# Drop variables
clean_dat <- na.omit(dat)
#glimpse(clean_dat)

shuffle_index <- sample(1:nrow(clean_dat))

data_train <- create_train_test(clean_dat, 0.8, train = TRUE)
data_test <- create_train_test(clean_dat, 0.8, train = FALSE)
dim(data_train)
dim(data_test)
prop.table(table(data_train$Outcome))

fit <- rpart(Outcome~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)


