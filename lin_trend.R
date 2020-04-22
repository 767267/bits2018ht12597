library(fpp2)

read_file <- function() {
  setwd("D:/BITS/svn")
  path = getwd()
  dat = read.csv("./qoc_data_1.csv", sep = ",", header = TRUE)
  return(dat)
}


y_data <-  read_file()

#yt_data <-  ts(y_data, start = c(2019,1),frequency = 12)

#yt_data <- y <- y_data[1,]

#fc <- holt(yt_data[,1:12], h=5)

#autoplot(y_data) +
  #autolayer(fc, series = "Holt's", PI = FALSE)
  
  
x <- y_data[,1]
y <- y_data[,2]

plot(x,y, type = 'o')
lm_mod <- lm( y ~ x)

summary(lm_mod)

abline(lm_mod)

