# whole data processing with k-means for prediction

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
source("get_filtered_data.r")


data <- get_filtered_data()

max_tries = 15

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:max_tries)
{
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

print(min(wss))


for(da in 2:9)
{
  mydata <- data[,1:da]
  
  # zero_preg <- count(mydata, mydata$Pregnancies == 0)
  # zero_preg["TRUE"]
  
 
  
  # 5 seems good
  
  no_of_clusters = 2
  
  run_kmeans <- function(xtimes)
  {
    kmeans <- lapply(seq_len(xtimes), function(i){
      results <- kmeans(mydata,  no_of_clusters)
    })
    return(kmeans)
  }
  
  kmeans_all_results = run_kmeans(10)
  
  perf <- sapply(kmeans_all_results, function(d) as.numeric(d["tot.withinss"]))
  index <- which.min(perf)
  kmeans_results = kmeans_all_results[[index]]
  
  df1<-data.frame(data,cluster=kmeans_results$cluster)
  
  for(i in 1:no_of_clusters)
  {
    dfc <- filter(df1, cluster == i)
    cat("c",i,":- Glucose range[", range(dfc$Glucose), "] BP range[",range(dfc$BloodPressure) ,"]\r\n")
  }
  
  sil <- silhouette(kmeans_results$cluster, dist(mydata))
  fviz_silhouette(sil)
}
#best <- filter(df1,df1$cluster==2)

