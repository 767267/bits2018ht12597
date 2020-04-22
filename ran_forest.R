# random forest
library(randomForest)
library(caret)
library(tidyverse)  # data manipulation

# install_reprtree  = 1
# if(install_reprtree == 1)
# {
#   options(repos='http://cran.rstudio.org')
#   have.packages <- installed.packages()
#   cran.packages <- c('devtools','plotrix','randomForest','tree')
#   to.install <- setdiff(cran.packages, have.packages[,1])
#   if(length(to.install)>0) install.packages(to.install)
#   
#   library(devtools)
#   if(!('reprtree' %in% installed.packages())){
#     install_github('araastat/reprtree')
#   }
#   for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))
#   
# }
# 
# library(reprtree)
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

nrow(train_data)
nrow(test_data)

str(train_data)


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
    

