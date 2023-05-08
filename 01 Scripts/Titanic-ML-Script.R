setwd("C:/Users/dsgra/OneDrive/Documents/GitHub/Titanic-Machine-Learning-from-Disaster")
library(tidyverse)
train = read.csv('C:/Users/dsgra/OneDrive/Documents/GitHub/Titanic-Machine-Learning-from-Disaster/00 Data/train.csv',
                 na.strings = c(""), stringsAsFactors=TRUE)
test = read.csv('C:/Users/dsgra/OneDrive/Documents/GitHub/Titanic-Machine-Learning-from-Disaster/00 Data/test.csv',
                na.strings = c(""), stringsAsFactors=TRUE)

#Data Preprocessing -------------------------------------------------------------------
#Convert columns into factors if they are factors
str(train)
train$Survived <- factor(train$Survived)
train$Pclass <- factor(train$Pclass)

#Before dropping columns and dealing with missing values take backup of original data
train_backup <- train
train[!complete.cases(train),]

#Cabin variable is nearly all NA, dropping as it is impossible to estimate cabin numbers
train <- subset(train,select = -c(Cabin))

#Age has 177 NAs, how to fill... Could replace with median,but let's wait and see.
table(train$Age, useNA = 'always')
summary(train$Age)
# table(train$PassengerId, useNA = 'always')
# table(train$Pclass, useNA = 'always')
# table(train$Name, useNA = 'always')
# table(train$Sex, useNA = 'always')
# table(train$SibSp, useNA = 'always')
# table(train$Parch, useNA = 'always')
# table(train$Ticket, useNA = 'always')
# table(train$Fare, useNA = 'always')
# Two passengers did not board the titanic, dropped
table(train$Embarked, useNA = 'always')
train <- train[!is.na(train$Embarked),]

str(test)
str(train)
test$Pclass <- factor(test$Pclass)
test_backup <- test
table(test$Cabin, useNA = 'always')
test <- subset(test,select = -c(Cabin))
#What to do about age...
table(train$Age, useNA = 'always')
summary(train$Age)
# Two passengers did not board the titanic, dropped
table(test$Embarked, useNA = 'always')
test <- add_column(.data = test, .before = 'Pclass')
#----------------------------------------------------------------------------------
# #Logistic regression
# classifier = glm(formula = Survived ~ .,
#                  family = binomial,
#                  data = train)
# 
# prob_pred = predict(classifier, type = 'response', newdata = test[-1])
# y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Fitting XGBoost to the Training set
library(xgboost)
data <- as.matrix(train[-2])
label <- as.numeric(train$Survived)
mode(data) <- 'double'
classifier = xgboost(data = data, label = label, nrounds = 10, objective = "binary:logistic")

# Predicting the Test set results
newdata = as.matrix(test[-11])
mode(newdata) <- 'double'
y_pred = predict(classifier, newdata = newdata)
y_pred = (y_pred >= 0.5)

y_pred
# Applying k-Fold Cross Validation
# install.packages('caret')
library(caret)
folds = createFolds(dataset$Exited, k = 10)
cv = lapply(folds, function(x) {
  training_fold = dataset[-x, ]
  test_fold = dataset[x, ]
  classifier = xgboost(data = data, label = label, nrounds = 10)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-11]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[, 11], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)