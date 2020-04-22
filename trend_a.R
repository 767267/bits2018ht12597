#library(forecast)
library(fpp2)
read_file <- function() {
  setwd("D:/BITS/svn")
  path = getwd()
  #print(path)
  dat = read.csv("./qoc_data.csv", sep = ",", header = FALSE)
  return(dat)
}
direction<-function(x){
  if(all(diff(x)>0)) return('Increasing')
  if(all(diff(x)<0)) return('Decreasing')
  return('Mixed')
}

data <- read_file()

#c1_data <- c(85,86,85,82,75,78,74,70,65,64,60,56)
#c2_data <- c(39,39,40,43,47,44,46,49,53,54,56,60)
#c3_data <- c(76,75,75,75,78,78,80,81,82,82,84,84)


#target <- c(10,10,10,10,10,10,10,10,10,10,10,10)

colors = c("red", "orange", "green")


#my.ts <- ts(c1_data, start = c(2019,1),frequency = 12)

#my.fore = HoltWinters(my.ts,beta = FALSE, gamma = FALSE)

#print(max(diff(my.trend), na.rm = TRUE))

#attributes(ts1[1])

x <- c(1:12)
y <- as.numeric(as.vector(data[1,]))
plot(x,y, ylim=c(30,100), xlab="Months", ylab = "Size of Clusters" )
# first cluster line - high risk
points(x,y,type='o', pch=20, col="red")
# plot the lin regression line to understand the slope
high_risk_model <- lm(y ~ x)
abline(high_risk_model, col=colors[1], lty = 2)
summary(high_risk_model)
for(i in 2:3)
{
x <- c(1:12)
y <- as.numeric(as.vector(data[i,]))

lines(x, y, col=colors[i],lty=2)
points(x,y,type='o', pch=20, col=colors[i])
abline(lm( y ~ x), col=colors[i], lty = 2) 

}

direction(as.numeric(data[1,]))
direction(as.numeric(data[2,]))
direction(as.numeric(data[3,]))


s1 <- ses(c1_data, h=12, alpha=0.1, initial="simple")


################
# library(signal)
# n <- 100
# x <- 1:n
# y <- ifelse(0.3*n < x & x < 0.7*n, 1, 0)
# par(mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
# plot(x, y, type='o', pch=20, ylim=c(-0.1, 1.1))
# 
# 
# W <- 0.1
# 
# b1 <- butter(1, W)
# y1 <- filtfilt(b1, y)
# points(x, y1, type='o', pch=20, col='red')
# 
# b2 <- butter(2, W)
# y2 <- filtfilt(b2, y)
# points(x, y2, type='o', pch=20, col='blue')
# 
# b3 <- butter(3, W)
# y3 <- filtfilt(b3, y)
# points(x, y3, type='o', pch=20, col='forestgreen')
# 
# legend("topright", lwd=2, pch=20, 
#        col=c("black", "red", "blue", "forestgreen"),
#        legend=c("Signal", "Order 1", "Order 2", "Order 3"))

