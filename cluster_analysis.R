library("factoextra")

source("read_diab_file.r")
source("get_filtered_data.r")
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

dat <- dat[,1:8]
# Finding optimal no. of clusters using k-means 
fviz_nbclust(dat, kmeans, method = "gap_stat")

# result we see optimal number is 1, in our case that will not make any siginificant results

km.res <- kmeans(dat, no_of_clusters, nstart = 25)
fviz_cluster(km.res, dat,  geom = "point", 
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_classic())

km.df1<-data.frame(dat,cluster=km.res$cluster)

for(xx in 1:no_of_clusters)
{
  dfc <- filter(km.df1, cluster == xx)
  
  cat("c",xx,":- Glucose range[", range(dfc$Glucose), "] BP range[",range(dfc$BloodPressure) ,"] Insulin range[",range(dfc$Insulin) ,"] BMI range[",range(dfc$BMI),"] Age range[",range(dfc$Age),"] ST range [",range(dfc$SkinThickness),"]  DPF range [",range(dfc$DiabetesPedigreeFunction),"]\r\n")
}
# Now DBSCAN 

set.seed(123)
res <- dbscan::kNNdistplot(dat, k =  3)
abline(h = 60, lty = 2)

db <- fpc::dbscan(dat, eps = 60, MinPts = 3)
print(db)
fviz_cluster(db, data = dat, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

summary(db$cluster)

km.df2<-data.frame(dat,cluster=db$cluster)
for(xx in 1:2)
{
  dfc <- filter(km.df2, cluster == xx)
  
  cat("c",xx,":- Glucose range[", range(dfc$Glucose), "] BP range[",range(dfc$BloodPressure) ,"] Insulin range[",range(dfc$Insulin) ,"] BMI range[",range(dfc$BMI),"] Age range[",range(dfc$Age),"] ST range [",range(dfc$SkinThickness),"]  DPF range [",range(dfc$DiabetesPedigreeFunction),"]\r\n")
}
# results we observe only 1 cluster
