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

no_of_clusters = 3

dat = read_diab_file()

dat =  filter(dat, Outcome == 1) 

filtered_data = dat[,c(2,4)]




cluster_colors <- c("red", "orange" , "green")

#print(dat[1])   dat %>% select(2:7) #

#filtered_data = dat[,c(2,3)] #dat[,c(4, 5,6)]

#count_na = sum(is.na(filtered_data))

#print(count_na)

#summary(filtered_data)

run_kmeans <- function(xtimes)
{
  min = 0
  #for (x in 1:xtimes) 
    #{
    #kmeans_results <- kmeans(filtered_data, no_of_clusters) 
    #print(x)
  #}   
  kmeans <- lapply(seq_len(xtimes), function(i){
    results <- kmeans(filtered_data, 3)
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

sil <- silhouette(kmeans_results$cluster, dist(filtered_data))
fviz_silhouette(sil)

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

mydata <- filtered_data

max_tries = 15

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:max_tries)
  {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

print(min(wss))
