#In-Class Exercise: KNN - Abalone

#abalone data from UCI repository
#reading the dataset from UCI repository URL
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header=FALSE, sep=",")
#Column names
colnames(abalone) <- c("sex", "length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight", "rings")
#summary on abalone
summary(abalone)
#structure of the abalone data
str(abalone)
#summary of the abalone rings column
summary(alabone$rings)

abalone$rings <- as.numberic(alabone$rings)
abalone$rings <- cut(abalone$rings, br=c(-1,8,11,35), labels=c("young", "adult", "old"))
abalone$rings <- as.factor(abalone$rings)
summary(abalone$rings)
aba <- abalone
aba$sex <- NULL 

#normalize data using min max normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))
summary(aba$shucked_weight)
#now each variable has min of 0 and max of 1

#train/set split
ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7,0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]
sqrt(2918)
#k = 55

#knn model
library(class)
help("knn")
KNNpred <- knn(train = KNNtrain[1:7], test=KNNtest[1:7], cl=KNNtrain$rings, k=55)
KNNpred
table(KNNpred)


#In-Class Exercise K-Means Iris dataset
library(ggplot2)
head(iris)
str(iris)
summary(iris)
#Petal.Length and Petal.Width
help("sapply")
sapply(iris[,-5],var)
summary(iris)
#plot Sepal.Length and Sepal.Width using ggplot
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, col=Species)) + geom_point()
#plot Petal.Length and Sepal.Width using ggplot
ggplot(iris, aex(s=Petal.Length, y=Petal.Width, col=Species)) + geom_point()
#kmeans clustering
#Read the documentation for kmeans() function
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html

set.seed(300)
k.max <- 12
# tot.withinss = Total within-cluster sum of square
# iter.max = the maximum number of iterations allowed
# nstart = if centers is a number, how many random sets should be chosen 
wss <- sapply(1:k.max, function(k){kmeans(iris[,3:4],k,nstart=20, iter.max=20)$tot.withinss})
wss #within sum of squares
plot(1:k.max, wss, type="b", xlab="Number of clusters (k)", ylab="Within cluster sum of squares")
icluster <- kmeans(iris[,3:4],3,nstart=20)
table(icluster$cluster, iris$Species)
# Most of the observations from the table have been clustered correctly 
# however, 2 of the versicolor have been put in the cluster with all the virginica
# and 4 of the verginica have been put in cluster 3 which mostly has versicolor.


#In-Class Exercises on Trees
# Classification ctrees
# iris data set
# Install the following libararies/packages library(rpart)
library(rpart.plot)
# we will be using the iris dataset
iris
dim(iris) # check the dimensions of the iris dataset
# creating a sample from the iris dataset s_iris <- sample(150,100)
s_iris
# creat testing and training sets iris_train <-iris[s_iris,]
iris_test <-iris[-s_iris,] dim(iris_test)
dim(iris_train)
# generate the decision tree model
dectionTreeModel <- rpart(Species~., iris_train, method = "class") dectionTreeModel
#plotting the decision tree model using rpart.plot() function rpart.plot(dectionTreeModel)


















