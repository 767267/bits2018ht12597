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
source("read_diab_file.r")
source("get_filtered_data.r")

raw = 0
no_of_clusters = 3

if(raw == 1)
{
  dat = read_diab_file()
}

if(raw == 0)
{
 dat = get_filtered_data()
}

summary(dat)

dim(dat)
#write.csv(dat, "./filtered_data.csv")

filter1 <- c(5,8,6)   # Insulin, Age, BMI > had good correlation with Glucose     - 0.71
filter2 <- c(5,8,6,3) # Insulin, Age, BMI, BP > had good correlation with Glucose - 0.66
filter3 <- c(5,8,6,4) # Insulin, Age, BMI, Skin Thickness > had good correlation with Glucose - 0.68
filter4 <- c(5,8,6,2) # Insulin, Age, BMI, Glucose

filter5 <- c(5,8,6,7)   # Insulin, Age, BMI, DPF

filter6 <- c(3,5,6)   # BP, Insulin, BMI

filter7 <- c(3,8,4,5)   # BP, Insulin, BMI

filter8 <- c(2,8,5)   # BP, Insulin, BMI

filter9 <- c(2,6,8)    # Glucose, BMI, Age

filter10 <- c(2,6,8 ,7)    # Glucose, BMI, Age, DPF

filtered_data = dat[,filter10]




#cluster_colors <- c("red", "orange" , "green")

#print(dat[1])   dat %>% select(2:7) #

#filtered_data = dat[,c(2,3)] #dat[,c(4, 5,6)]

#count_na = sum(is.na(filtered_data))

#print(count_na)

#summary(filtered_data)

run_kmeans <- function(xtimes)
{
  kmeans <- lapply(seq_len(xtimes), function(i){
    results <- kmeans(filtered_data,  no_of_clusters)
  })
  return(kmeans)
}

kmeans_all_results = run_kmeans(10)

perf <- sapply(kmeans_all_results, function(d) as.numeric(d["tot.withinss"]))
index <- which.min(perf)

kmeans_results = kmeans_all_results[[index]]

#print(kmeans_results["totss"]) #iter.max = 10, nstart = 1, algorithm = "Hartigan-Wong"
#print(kmeans_results["tot.withinss"])
#print(kmeans_results["betweenss"])

#kmeans_results <- kmeans(filtered_data, 3)

fviz_cluster(kmeans_results, data = filtered_data, iter.max = 10, nstart = 1, algorithm = "Hartigan-Wong", ellipse.type = "norm") #+
  #scale_colour_manual(values = c(cluster_colors[1], cluster_colors[2], cluster_colors[3])) +
  #scale_fill_manual(values = c(cluster_colors[1], cluster_colors[2], cluster_colors[3])) 

c_len = length(kmeans_results$cluster)

print(c_len)

df1<-data.frame(dat,cluster=kmeans_results$cluster)



for(xx in 1:no_of_clusters)
{
  dfc <- filter(df1, cluster == xx)
 
  cat("c",xx,":- Glucose range[", range(dfc$Glucose), "] BP range[",range(dfc$BloodPressure) ,"] Insulin range[",range(dfc$Insulin) ,"] BMI range[",range(dfc$BMI),"] Age range[",range(dfc$Age),"] ST range [",range(dfc$SkinThickness),"]  DPF range [",range(dfc$DiabetesPedigreeFunction),"]\r\n")
}
sil <- silhouette(kmeans_results$cluster, dist(filtered_data))
fviz_silhouette(sil)

# hists <- lapply(seq_len(3), function(i){
#   dff <- filter(df1, cluster == i)
#   hists <- hist(dff$Age)
# })

#dfc1 <- filter(df1, cluster == 1)
#dfc2 <- filter(df1, cluster == 2)
#dfc3 <- filter(df1, cluster == 3)
# hist(dfc1$Glucose)
# hist(dfc2$Glucose)
# hist(dfc3$Glucose)

# c31 = 0
# c30 = 0
# c21 = 0
# c20 = 0
# c11 = 0
# c10 = 0
# 
# 
# for(i in 1:c_len) {
#   if(kmeans_results$cluster[i] == 3)
#   {
#     if(dat[i,c("Outcome")] == 1)
#     {
#       c31 = c31 + 1
#     }
#     else
#     {
#       c30 = c30 + 1
#     }
#   }
#   else if(kmeans_results$cluster[i] == 2)
#   {
#     if(dat[i,c("Outcome")] == 1)
#     {
#       c21 = c21 + 1
#     }
#     else
#     {
#       c20 = c20 + 1
#     }
#   }
#   else
#   {
#     if(dat[i,c("Outcome")] == 1)
#     {
#       c11 = c11 + 1
#     }
#     else
#     {
#       c10 = c10 + 1
#     }
#   }
# 
# }
# 
# print(c31)
# print(c30)
# 
# print(c21)
# print(c20)
# 
# print(c11)
# print(c10)


# finding outliers using box plots on Glucose, 

#boxGlucose <- boxplot(dat$Glucose,dat$BloodPressure,dat$SkinThickness)
#boxBloodPressure <- boxplot(dat$Insulin)
#newinsulin <- dat$Insulin[dat$Insulin > 350]
#boxBloodPressure <- boxplot(newinsulin)
#hist(newinsulin)



# https://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters




