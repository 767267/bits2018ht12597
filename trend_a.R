# Trend understanding using lm regression
# lm is choosen as the data changes over one year times are not significant 

read_file <- function() {
  dat = read.csv("./qoc_data.csv", sep = ",", header = FALSE)
  return(dat)
}
direction<-function(x){
  if(all(diff(x)>0)) return('Increasing')
  if(all(diff(x)<0)) return('Decreasing')
  return('Mixed')
}

data <- read_file()

colors = c("red", "orange", "green")


x <- c(1:12)
y <- as.numeric(as.vector(data[1,]))
plot(x,y, ylim=c(30,100), xlab="Months", ylab = "Size of Clusters" )
# first cluster line - high risk
points(x,y,type='o', pch=20, col="red")
# plot the lin regression line to understand the slope
high_risk_model <- lm(y ~ x)

n <- 3
mat <- matrix(ncol=2, nrow=n)
colnames(mat) <- c("Intercept", "Slope")
rownames(mat) <- c("high risk", "moderate risk" , "low risk")
mat[1,]  <- coef(high_risk_model)

abline(high_risk_model, col=colors[1], lty = 2)
#summary(high_risk_model)


for(i in 2:3)
{
  x <- c(1:12)
  y <- as.numeric(as.vector(data[i,]))
  
  lines(x, y, col=colors[i],lty=2)
  points(x,y,type='o', pch=20, col=colors[i])
  
  temp_model <- lm(y ~ x)
  mat[i,] <- coef(temp_model)
  abline(temp_model, col=colors[i], lty = 2) 
  
}

print(mat)

#direction(as.numeric(data[1,]))
#direction(as.numeric(data[2,]))
#direction(as.numeric(data[3,]))


##
##s1 <- ses(c1_data, h=12, alpha=0.1, initial="simple")

