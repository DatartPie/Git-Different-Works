library(datasets)
library(xgboost)
data(iris)
a = as.matrix(iris[, 1:4]) #4 columns 
b = as.numeric(factor(iris[, 5]))-1 #labels
#no test data but used cross validation to display the function of cross validation in XGBoost itself
mod_1 <- xgboost(data = a, label = b, nrounds = 20)  #simple xgb model1 with mandatory parameters & rmse as measure
set.seed(5) #produce same simulation
cross_val1 <- xgb.cv(data = a, label = b, nfold = 5, nrounds = 20) #cross validation to assess performance of model
set.seed(5)
cross_val2 <- xgb.cv(data = a, label = b, nfold = 5, nrounds = 100) # to see behaviour of test rmse measure
cross_val3 <- xgb.cv(data = a, label = b, nfold = 5, nrounds = 100, early_stopping_rounds = 10) #early stopping round to stop our algorithm when it finds a decreasing measure

