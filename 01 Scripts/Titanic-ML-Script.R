setwd("C:/Users/dsgra/OneDrive/Documents/GitHub/Titanic-Machine-Learning-from-Disaster")
library(tidyverse)

train <- readr::read_csv('C:/Users/dsgra/OneDrive/Documents/GitHub/Titanic-Machine-Learning-from-Disaster/00 Data/train.csv')
test <- readr::read_csv('C:/Users/dsgra/OneDrive/Documents/GitHub/Titanic-Machine-Learning-from-Disaster/00 Data/test.csv')

# Fitting XGBoost to the Training set
# install.packages('xgboost')
library(xgboost)
classifier = xgboost(data = as.matrix(train[2]), label = train$Survived, nrounds = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = as.matrix(test[1,11]))
y_pred = (y_pred >= 0.5)

# Making the Confusion Matrix
cm = table(test_set[, 11], y_pred)

# Applying k-Fold Cross Validation
# install.packages('caret')
library(caret)
folds = createFolds(dataset$Exited, k = 10)
cv = lapply(folds, function(x) {
  training_fold = dataset[-x, ]
  test_fold = dataset[x, ]
  classifier = xgboost(data = as.matrix(training_fold[-11]), label = training_fold$Exited, nrounds = 10)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-11]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[, 11], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)