
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


library(corrplot)
library(tidyverse)
source("read_diab_file.r")

dat = read_diab_file()

#define Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#apply Min-Max normalization to first four columns in iris dataset
dat_norm <- as.data.frame(lapply(dat[2:6], min_max_norm))

#dat_norm1 = normalize(dat, method = "standardize", range = c(5), margin = 1L, on.constant = "quiet")

boxplot(dat_norm)

corrplot.mixed(cor(dat_norm), order="hclust", tl.col="black")
