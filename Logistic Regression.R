#Read Data File
mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

#Summary
summary(mydata)

#Cross Tab
xtabs(~admit + rank, data = mydata)

#Data Preparation
mydata$rank <- factor(mydata$rank)

# Split data into training (70%) and validation (30%)
dt = sort(sample(nrow(mydata), nrow(mydata)*.7))
train<-mydata[dt,]
val<-mydata[-dt,] 

# Check number of rows in training and validation data sets
nrow(train)
nrow(val)

#Run Logistic Regression
mylogistic <- glm(admit ~ ., data = train, family = "binomial")
summary(mylogistic)$coefficient

#Stepwise Logistic Regression
mylogit = step(mylogistic)

#Logistic Regression Coefficient
summary.coeff0 = summary(mylogit)$coefficient

#Calculating Odd Ratios
OddRatio = exp(coef(mylogit))
summary.coeff = cbind(Variable = row.names(summary.coeff0), OddRatio, summary.coeff0)
row.names(summary.coeff) = NULL

#R Function : Standardized Coefficients
stdz.coff <- function (regmodel) 
{ 
  b <- summary(regmodel)$coef[-1,1]
  sx <- sapply(regmodel$model[-1], sd)
  beta <- (3^(1/2))/pi * sx * b
  return(beta)
}
#debug(stdz.coff)
std.Coeff = data.frame(Standardized.Coeff = stdz.coff(mylogit))
#undebug(stdz.coff)
std.Coeff = cbind(Variable = row.names(std.Coeff), std.Coeff)
row.names(std.Coeff) = NULL

#Final Summary Report
final = merge(summary.coeff, std.Coeff, by = "Variable", all.x = TRUE)

#Prediction
pred = predict(mylogit,val, type = "response")
finaldata = cbind(val, pred)
for(i in 1:nrow(finaldata)){
  if(finaldata$pred[i] < .5){
    finaldata$admit_pred[i] = "0"
  }else{
    finaldata$admit_pred[i] = "1"
  }
}
for(i in 1:nrow(finaldata)){
  if(finaldata$admit_pred[i] == finaldata$admit[i]){
    finaldata$Result[i] = "SAME" 
  }else{
    finaldata$Result[i] = "DIFFERENT" 
  }
}
#Storing Model Performance Scores
install.packages("ROCR")
library(ROCR)
pred_val <-prediction(pred ,finaldata$admit)

# Maximum Accuracy and prob. cutoff against it
acc.perf <- performance(pred_val, "acc")
ind = which.max( slot(acc.perf, "y.values")[[1]])
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]

# Print Results
print(c(accuracy= acc, cutoff = cutoff))

# Calculating Area under Curve
perf_val <- performance(pred_val,"auc")
perf_val

# Plotting Lift curve
plot(performance(pred_val, measure="lift", x.measure="rpp"), colorize=TRUE)

# Plot the ROC curve
perf_val2 <- performance(pred_val, "tpr", "fpr")
plot(perf_val2, col = "green", lwd = 1.5)

