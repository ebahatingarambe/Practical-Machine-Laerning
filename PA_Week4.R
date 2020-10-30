## Download data

fileUrl_training <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
fileUrl_testing <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(fileUrl_training, destfile = "training.csv")
download.file(fileUrl_testing, destfile = "testing.csv")

### Load data and libraries
library(caret); library (ggplot2); library (lattice)

## Remove NAs in the database
my_training <- read.csv("training.csv", na.strings = c("NA", ""))
my_testing <- read.csv("testing.csv", na.strings = c("NA", ""))

## Exploratory data analysis
dim(my_training); dim(my_testing)

## Data cleaning
#### 1. Subset columns to use in the prediction exercise
training <- my_training[,-c(1:7)]
testing <- my_testing[,-c(1:7)]

#### 2. Removing variables with near zero variance
nearZeroVariance <- nearZeroVar(testing)
training <- training[, -nearZeroVariance]
testing <- training[, -nearZeroVariance]

## Separate training dataset to increase performance and accuracy
# create training and testing date sets
inTrain<- createDataPartition(y= training$classe, p=0.75, list=FALSE)
training_train<- training[inTrain, ]
training_test<- training[-inTrain, ]

#Look at the dimension of boths data sets
dim(training_train); dim(training_test)

## Predicition
library(randomForest)
# convert the classe data type from character to factor variable
training_train$classe <- factor(training_train$classe)
training_test$classe <- factor(training_test$classe)

### Random Forest Model
modfit_rf <- train (classe ~., data = training_train, method = "rf")

#Prediction 
predict_rf <- predict(modfit_rf, training_test)

# Accuracy measurement
confMatrix_rf <- confusionMatrix(predict_rf, training_test$classe)
confMatrix_rf

## Apply the random forest model to the testing data set
final_predict_rf <- predict(modfit_rf, my_testing)
final_predict_rf














