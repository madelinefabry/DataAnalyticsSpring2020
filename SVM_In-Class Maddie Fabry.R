#In-Class Work Example: SVM

#SVM Classification Example
#Support Vector Machine (SVM) example using iris dataset 
data("iris")
head(iris) #inspecting the first six rows of the datset
str(iris) #structure of the dataset
library(ggplot2)
library(e1071)

#We will separate the Spcies based on the color and plot with Petal.Length vs Petal.Width
#We can clearly see the separation of Setosa however, there is an overlappings of Versicolor and Virginica
#Now we plot using the qplot() function, X=Petal.Length and Y=Petal.Width
#using the color separation respect to Species
qplot(Petal.Length, Petal.Width, data=iris, color=Species)

#Now we can use the built in svm() funciton that come in the e1071 library
#here we will name our first svm model as svm_model1
#read the svm() documentation on RStudio by using the help(svm) function
help("svm")
svm_model1 <- svm(Species~., data=iris)

#Using the summary command to see the summary of our first model, we can pass the svm_model1 to the 
#summary() function
summary(svm_model1)

#Using the plot() function and our first model with is the svm_model1 we can plot the results
#here the axes are Petal.Width vs Petal.Length
plot(svm_model1, data=iris, Petal.Width~Petal.Length, slice=list(Sepal.Width = 3, Sepal.Length=4))
#Predicted class regions are provided with the color background

#Prediction using the model (svm_model1) we created on the iris dataset
pred1 <- predict(svm_model1, iris)
#creating a table using the one predicted and the actual iris dataset
table1 <- table(Predicted = pred1, Actual = iris$Species)
table1
#two misclassifications of Virginica and Versicolor

#We can calculate the model1 accuracy
Model1_accuracyRate = sum(diag(table1))/sum(table1)
Model1_accuracyRate
#We can calculate the misclassification rate
Model1_MisclassificationRate = 1 - Model1_accuracyRate
Model1_MisclassificationRate

#----Model 2 ----
#Now we will use other methods such as linear, polynomial...for kernals
#kernal = "linear"
svm_model2 <- svm(Species~., data=iris, kernal="linear")

#Using the summary command to see the usmmary of our second model,
#we pass the svm_model2 to the
#summary() function
summary(svm_model2)

#Using the plot() function and our second model (svm_model2), we can plot the results
#here the axes are Petal.Width vs Petal.Length
plot(svm_model2, data=iris, Petal.Width~Petal.Length, slice=list(Sepal.Width=3, Sepal.Length=4))

#Prediction using the second model (svm_model2) we created on the iris dataset
pred2 <- predict(svm_model2, iris)
#creating a table using the one predicted and the actual iris dataset
table2 <- table(Predicted=pred2, Actual=iris$Species)
table2
#different classifications compared to model1

#We can calculate the model2 accuracy
Model2_accuracyRate = sum(diag(table2))/sum(table2)
Model2_accuracyRate
#We can calculate the misclassification rate
Model2_MisclassificationRate = 1 - Model2_accuracyRate
Model2_MisclassificationRate


#----Model 3 ----
#kernal = "polynomial"
svm_model3 <- svm(Species~., data=iris, kernal="polynomial")
summary(svm_model3)
plot(svm_model3, data=iris, Petal.Width~Petal.Length, slice=list(Sepal.Width=3, Sepal.Length=4))
pred3 <- predict(svm_model3, iris)
table3 <- table(Predicted=pred3, Actual=iris$Species)
table3
Model3_accuracyRate = sum(diag(table3))/sum(table3)
Model3_accuracyRate
Model3_MisclassificationRate = 1 - Model3_accuracyRate
Model3_MisclassificationRate





























