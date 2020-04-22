#find best corelations 

#devtools::install_github("neuropsychology/psycho.R")  # Install the newest version

#library(psycho)  
#library("PerformanceAnalytics")

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
library(BBmisc)

source("read_diab_file.r")
source("get_filtered_data.r")


raw = 0

if(raw == 1)
{
  dat = read_diab_file()
}
if(raw == 0)
{
  dat = get_filtered_data()
  
}
# 
# dim(dat)
# 
# hist(dat[,"Insulin"])


#display(dat.info(),dat.head())

correlations = cor(dat[,2:8])
write.csv(correlations, "./correlations.csv")


corrplot.mixed(correlations, order="hclust", tl.col="black")

#box plots for outlier analysis

outGlucose <- boxplot(dat[,2:7])


outGlucose1 <- boxplot(dat[,"Glucose"])

hist(dat[,"Glucose"])

outInsulin <- boxplot(dat[,"Insulin"])



hist(dat[,"Insulin"])

#count(dat, "Insulin", 0)


valid_dat =  filter(dat, Glucose > 70 )

count(valid_dat)

hist(valid_dat[,"Glucose"])

correlations1 <- cor(valid_dat[,2:8])

#hist(valid_dat[,"Insulin"])

corrplot.mixed(correlations1, order="hclust", tl.col="black")


dat3 = filter(valid_dat, Insulin > 0 )

corrplot.mixed(cor(dat3[,2:8]), order="hclust", tl.col="black")

min(dat3[,"Age"])

max(dat3[,"Age"])

hist(dat[,"Age"])

summary(dat3)


