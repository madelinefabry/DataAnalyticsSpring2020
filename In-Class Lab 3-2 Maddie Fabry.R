#Validation set example with Auto dataset

#Random Forest Example

#Car Acceptability (Car Conditions) dataset from UCI ML Repository
#https://archive.ics.uci.edu/ml/machine-learning-databases/car/
install.packages("randomForest")
library(randomForest)

#Loading the dataset
data1 <- read.csv(file.choose(), header=TRUE)
head(data1)
#Adding the column names:
colnames(data1) <- c("BuyingPrice", "Maintenance", "NumDoors", "NumPersons", "BootSpace", "Safety", "Condition")
head(data1)
str(data1)

#Levels of Condition column
#Condition levels: "acc" "good" "unacc" "vgood"
levels(data1$Condition)
summary(data1)

#Creating the "training dataset" and "Validation dataset" 
#Randomly choose 70% of the data points for training, 30% for validation
#set seed
set.seed(100)
train <- sample(nrow(data1), 0.7*nrow(data1), replace=FALSE)
TrainSet <- data1[train,]
ValidSet <- data1[-train,]
summary(TrainSet)
summary(ValidSet)

help(randomForest)
#Random Forest Model with default parameters
model1 <- randomForest(Condition ~ ., data=TrainSet, importance=TRUE)
model1
#Default: number of tress = 500 and number of variables tried at each split is 2 in this case

#Fine tuning the parameters of the RandomForest model
#increased the mtry to 6 from 2
#mtry = Number of variables randomly sampled as candidates at each split
#Different default values for classification (sqrt(p) where p is the number of variables in x) and regression (p/3)
model2 <- randomForest(Condition ~ ., data=TrainSet, ntree=500, mtry=6, importance=TRUE)
model2

#Conducting prediction using training set then on Validation set
#Predicting on the training dataset
predTrain <- predict(model2, TrainSet, type="class")
table(predTrain, TrainSet$Condition)
#Predicting on validaiton dataset
predValid <- predict(model2, ValidSet, type="class")
table(predValid, ValidSet$Condition)

#importance() function will be used to check important variables
importance(model2)
varImpPlot(model2)

#for loop to check difference values of mtry
#Will identify the right 'mtry' for the model
a = c()
i = 5
for (i in 3:8) {
  model3 <- randomForest(Condition~., data=TrainSet, ntree=500, mtry=i, importance=TRUE)
  predValid <- predict(model3, ValidSet, type="class")
  a[i-2] = mean(predValid = ValidSet$Condition)
}
a
plot(3:8,a)

#Implementation of Random Forest and understanding importance of the model
#Compare this model with decision tree and see how decision trees fare in comparison to random forest
#Comare with Decision Tree
library(rpart)
library(caret)
library(e1071)

#Comparing model1 of Random Forest with Decision Tree model
model_dt <- train(Condition ~., data=TrainSet, method="rpart")
model_dt_1 = predict(model_dt, data=TrainSet)
table(model_dt_1, TrainSet$Condition)
mean(model_dt_1 == TrainSet$Condition)
table(model_dt_1, TrainSet$Condition)
mean(model_dt_1 == TrainSet$Condition)

#accuracy at about 79.4% with lots of misclassification 

#Validation Set
model_dt_vs = predict(model_dt, newdata=ValidSet)
table(model_dt_vs, ValidSet$Condition)
mean(model_dt_vs == ValidSet$Condition)

