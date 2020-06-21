rm(list=ls()); gc()
library(rpart)
options(warn=-1) #function to suppress warnings 
library(caTools)#library used to split the dataset into train and test sets
library(rpart.plot)
library(caret) #library that provides KNN function 
setwd("C:/Users/reddy/Desktop/Masters/ISDS 574/Project/")
data = read.csv('MLR_Data.csv', stringsAsFactors=T, head=T)
data
set.seed(123)

#splitting the data into train and test set 
split = sample.split(data$price, SplitRatio = 0.7)
train = subset(data, split==TRUE)
test = subset(data, split==FALSE)

#Function to calculate rmse for models
rmse_reg <- function(model_obj, testing = NULL, target = NULL) {
  #Calculates rmse for a regression decision tree
  #Arguments:
  # testing - test data set
  # target  - target variable (length 1 character vector)
  yhat <- predict(model_obj, newdata = testing)
  actual <- testing[[target]]
  sqrt(mean((yhat-actual)^2))
}
################################################
# Rpart Regression Tree Example


rpart_fit = rpart(price ~ ., method="anova", data=train)
rpart.plot(rpart_fit, main = 'Regression Tree')
printcp(rpart_fit) # display the results
plotcp(rpart_fit) # visualize cross-validation results
summary(rpart_fit) # detailed summary of splits

pfit = prune(rpart_fit, cp = rpart_fit$cptable[which.min(rpart_fit$cptable[,"xerror"]),"CP"])
rpart.plot(pfit, main = 'Min Error Tree')

#rmse for rpart
rmse_reg(rpart_fit, test, "price")

################################################
# Linear Regression  Example
linreg_fit=lm(price~.,data=train)
summary(linreg_fit)

#rmse for linear regression
rmse_reg(linreg_fit, test, "price")

################################################
# KNN Regression  Example
x_train=train[,-1]
x_test=test[,-1]
y_train=train[,1]
y_test=test[,1]
knn_fit <- knnreg( x_train, y_train, k = 5)

predicted_value= predict(knn_fit, x_test)

#rmse for knn
rmse_knn=sqrt(mean((predicted_value-y_test)^2))
print(rmse_knn)
