setwd("~/Documents/Grad School Sem2/Data Analytics/Assignment 7")
install.packages("gains")
library(gains)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(dplyr)
library(fastDummies)

#PART 1
#DATASET 1: Classification and Regression
#Issue with data, so directly saved CSV file from Excel Spreadsheet for better formatting
absentee <- read.csv("Absenteeism_at_work1.csv")
absentee
attach(absentee)
fix(absentee)

#Data summary
abs1<-data.frame(absentee)
head(abs1)
dim(abs1)
#740 people for 21 parameters
names(abs1)
nrow(abs1)
ncol(abs1)
str(abs1)
#all factors are integer values, except for Work.load.Average.day
summary(abs1)
#Discrete data: ID, month, day of week, seasons, age, disciplinary failure, education, son, social drinker, social smoker, pet
#Continguous data: transportation expense, distance from residence to work, service time, weight, height, BMI, absenteeism time in hours
#Factor variables (Categorical): reason for abs, education, social drinker, social smoker, pet
#INCLUDE SUMMARY STATS 
names(abs1)

#distributions
shapiro.test(abs1$Month.of.absence)
shapiro.test(abs1$Day.of.the.week)
shapiro.test(abs1$Seasons)
shapiro.test(abs1$Transportation.expense)
shapiro.test(abs1$Distance.from.Residence.to.Work)
shapiro.test(abs1$Service.time)
shapiro.test(abs1$Age)

shapiro.test(abs1$Work.load.Average.day)
#is not numeric: needs to transform from factor to numeric class
class(abs1$Work.load.Average.day)
abs1$Work.load.Average.day <-as.numeric(abs1$Work.load.Average.day)

shapiro.test(abs1$Work.load.Average.day)
shapiro.test(abs1$Hit.target)
shapiro.test(abs1$Education)
shapiro.test(abs1$Disciplinary.failure)
shapiro.test(abs1$Son)
shapiro.test(abs1$Social.drinker)
shapiro.test(abs1$Social.smoker)
shapiro.test(abs1$Pet)
shapiro.test(abs1$Weight)
shapiro.test(abs1$Height)
shapiro.test(abs1$Body.mass.index)
shapiro.test(abs1$Absenteeism.time.in.hours)
#All factors are normally distributed

boxplot(abs1$Work.load.Average.day)
boxplot(abs1$Hit.target)
boxplot(abs1$Education)
boxplot(abs1$Disciplinary.failure)
boxplot(abs1$Son)
boxplot(abs1$Social.drinker)
boxplot(abs1$Social.smoker)
boxplot(abs1$Pet)
#outliers here: could be important if Pet is a principle component
boxplot(abs1$Weight)
boxplot(abs1$Height)
#outliers here: could be important if Pet is a principle component
boxplot(abs1$Body.mass.index)
boxplot(abs1$Absenteeism.time.in.hours)
#contains the most outliers 

#plotting reason for absence vs absenteeism 
ggplot(data=abs1) + (aes(x = Reason.for.absence, y = Absenteeism.time.in.hours)) +
  geom_col()

summary(abs1)
pca_out <- prcomp(abs1, scale. = T)
pca_out
plot(pca_out)
names(pca_out)

principal_components <- princomp(abs1, cor = TRUE, score = TRUE)
summary(principal_components)

#exploring distribution: Absenteeism of Employees
par(mar=c(1,1,1,1))
fivenum(abs1$Absenteeism.time.in.hours)
stem(abs1$Absenteeism.time.in.hours)
boxplot(abs1$Absenteeism.time.in.hours)
#contains outliers -> could create heatmap of absenteeism compared to other parameters
#should not remove outliers, since will be output parameter in regression
hist(abs1$Absenteeism.time.in.hours)
hist(abs1$Absenteeism.time.in.hours, seq(0., 125, 10.0), prob=TRUE)
#distribution = positively skewed
lines(density(abs1$Absenteeism.time.in.hours,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(abs1$Absenteeism.time.in.hours)

plot(ecdf(abs1$Absenteeism.time.in.hours), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(abs1$Absenteeism.time.in.hours)
qqline(abs1$Absenteeism.time.in.hours)
x <- seq(0, 125, 10)
qqplot(qt(ppoints(250), df=5), x, xlab='Q-Q plot for t dist')
qqline(x)

#DATASET 2: WHITE WINE
wine <- read.csv("winequality-white.csv")
wine

wine1<-data.frame(wine)
head(wine1)
dim(wine1)
#4898 wine types with 13 parameters
names(wine1)
nrow(wine1)
ncol(wine1)
str(wine1)
#ID and quality are integer, all others are numeric
summary(wine1)
names(wine1)

shapiro.test(wine1$fixed.acidity)
shapiro.test(wine1$volatile.acidity)
shapiro.test(wine1$citric.acid)
shapiro.test(wine1$residual.sugar)
shapiro.test(wine1$chlorides)
shapiro.test(wine1$free.sulfur.dioxide)
shapiro.test(wine1$total.sulfur.dioxide)
shapiro.test(wine1$density)
shapiro.test(wine1$pH)
shapiro.test(wine1$sulphates)
shapiro.test(wine1$alcohol)
#all tests pass the shapiro test

boxplot(wine1$fixed.acidity)
hist(wine1$fixed.acidity)
boxplot(wine1$volatile.acidity)
hist(wine1$volatile.acidity)
boxplot(wine1$citric.acid)
hist(wine1$citric.acid)
boxplot(wine1$residual.sugar)
hist(wine1$residual.sugar)
boxplot(wine1$chlorides)
hist(wine1$chlorides)
boxplot(wine1$free.sulfur.dioxide)
hist(wine1$free.sulfur.dioxide)
boxplot(wine1$total.sulfur.dioxide)
hist(wine1$total.sulfur.dioxide)
boxplot(wine1$density)
hist(wine1$density)
boxplot(wine1$pH)
hist(wine1$pH)
boxplot(wine1$sulphates)
hist(wine1$sulphates)
boxplot(wine1$alcohol)
hist(wine1$alcohol)

#exploring distribution: Alcohol in Wine
par(mar=c(1,1,1,1))
fivenum(wine1$alcohol)
stem(wine1$alcohol)
boxplot(wine1$alcohol)
#does not contains outliers 
hist(wine1$alcohol)
hist(wine1$alcohol, seq(0., 15, 1.0), prob=TRUE)
#distribution = normal
lines(density(wine1$alcohol,na.rm=TRUE)) # or try bw=“SJ”
rug(wine1$alcohol)

plot(ecdf(wine1$alcohol), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(wine1$alcohol)
qqline(wine1$alcohol)
x <- seq(0, 15, 1.0)
qqplot(qt(ppoints(250), df=5), x, xlab='Q-Q plot for t dist')
qqline(x)

#########################################
#PART 2


#MODEL 1 
#Regression: Multivariate Linear Regression to determine absenteeism 
set.seed(1)

lm_abs<- lm(Absenteeism.time.in.hours~., data=abs1)
lm_abs
summary(lm_abs)
#need to make dummy variables for factors
#significant factors = reason for absence, days of the week, disciplinary failure, education, son at 0.01 significance
#rerun linear regression with these parameters


results1 <- fastDummies::dummy_cols(abs1, select_columns=c("Day.of.the.week", "Reason.for.absence", "Education"))
results1
class(results1)
class(abs1)
results2 <- data.frame(abs1, results1)
dim(results2)
names(results2)
abs2 <- results2[,c(1, 3:12, 14:21, 43:79)]
abs2

# Partitioning into training (80%) and validation (20%)
train.index <- sample(c(1:dim(abs2)[1]), dim(abs2)[1]*0.8)  
train <- abs2[train.index, ]
valid <- abs2[-train.index, ]


lm_abs1<- lm(Absenteeism.time.in.hours~.,
            data=train)
lm_abs1
summary(lm_abs1)

#remove insignificant parameters

#coefficients
c_lm_abs1<-coef(lm_abs1)
c_lm_abs1
round(data.frame(summary(lm_abs1)$coefficients, odds = exp(coef(lm_abs1))), 5)
#MSE Value = 176
mean(lm_abs1$residuals^2)

#Validation & Prediction
library(caret)
library(dplyr)
pred <- predict(lm_abs1, valid, se.fit=TRUE, interval="prediction")
pred
summary(pred)
#standard error of predicted mean = 148
data.frame( R2 = R2(pred$fit, valid$Absenteeism.time.in.hours),
            RMSE = RMSE(pred$fit, valid$Absenteeism.time.in.hours),
            MAE = MAE(pred$fit, valid$Absenteeism.time.in.hours))

#####
#MODEL 2
#Classification: Decision Trees for disciplinary failure
names(abs1)
abs1_slice <- abs1[,c(6:9,11:20)] 

summary(abs1_slice)
# Partitioning into training (80%) and validation (20%)
train.index <- sample(c(1:dim(abs1)[1]), dim(abs1)[1]*0.8)  
train <- abs1[train.index, ]
valid <- abs1[-train.index, ]

heatmap(as.matrix(abs1_slice), Colv = NA, hclustfun = hclust)
cor(x=abs1_slice$Disciplinary.failure, y=abs1_slice)
#Decision Tree Model
TreeModel <- rpart(Disciplinary.failure~.,
                   data=train,
                   method = "class",
                   cp=0,
                   minsplit=1)
TreeModel
summary(TreeModel)
rpart.plot(TreeModel, fallen.leaves = FALSE)
#Prediction
model.pred.train <- predict(TreeModel, train, type = "class")
head(model.pred.train)

#confusion matrix
confusionMatrix(model.pred.train, as.factor(train$Disciplinary.failure))

#Validation
model.pred.valid <- predict(TreeModel, valid, type = "class")
confusionMatrix(model.pred.valid, as.factor(valid$Disciplinary.failure))
printcp(TreeModel)
plotcp(TreeModel)

# Create a lift chart
pred$fit
length(valid$Disciplinary.failure)
length(pred$fit)
gain<-gains(valid$Disciplinary.failure, as.numeric(model.pred.valid))
gain

#####
#MODEL 3
#Clustering
# Partitioning into training (80%) and validation (20%)
train.index <- sample(c(1:dim(abs1)[1]), dim(abs1)[1]*0.8)  
train <- abs1[train.index,]
valid <- abs1[-train.index,]

abs2

d.norm <- dist(train, method="euclidean")
max(d.norm)
par(mar=c(1,1,1,1))
#hierarchical agglomerative clustering
#Dendogram for Single Linkage
#hclust() methods: "ward.D", "single", "complete", "average", "median", or "centroid"
#single linkage
hc1 <- hclust(d.norm, method="single")
plot(hc1, hang=-1, ann=FALSE, main="Single Linkage Dendrogram", xlab="Wines")
#average linkage
hc2 <- hclust(d.norm, method="average")
plot(hc2, hang=-4, ann=FALSE,  main="Average Linkage Dendrogram", xlab="Wines")
#ward's method
hc3 <- hclust(d.norm, method="ward.D")
plot(hc3, hang=-4, ann=FALSE,  main="Ward's Dendrogram", xlab="Wines")
#clustering via cutting dendrogram: should be in three groups
#ward's method
memb <- cutree(hc3, k=3)
memb
#creating heatmap
row.names(train) <- paste(memb, ": ", row.names(train), sep="")
heatmap(as.matrix(train), Colv = NA, hclustfun = hclust)
#run kmeans algorithm
set.seed(2)
#look at set seed documentation
km <- kmeans(abs2, 3)
summary(km)
#cluster membership
km$cluster
#centroids
km$centers
#most differing values = chlorides, free sulfur dioxide, and residual sugar
#within-cluster sum of squares
km$withinss
#cluster size
km$size
#labeling x axis for features
par(mar=c(1,1,1,1))
axis(1, at=c(1:11), labels=names(abs2))
#between cluster distance
km$betweenss
km



#####
#MODELING 4
#Clustering
# Partitioning into training (80%) and validation (20%)
wine2 <- wine1[,c(2:12)]
train.index <- sample(c(1:dim(wine2)[1]), dim(wine2)[1]*0.8)  
train <- wine2[train.index, ]
valid <- wine2[-train.index, ]

d.norm <- dist(train, method="euclidean")
max(d.norm)
par(mar=c(1,1,1,1))
#hierarchical agglomerative clustering
#Dendogram for Single Linkage
#hclust() methods: "ward.D", "single", "complete", "average", "median", or "centroid"
#single linkage
hc1 <- hclust(d.norm, method="single")
plot(hc1, hang=-1, ann=FALSE, main="Single Linkage Dendrogram", xlab="Wines")
#average linkage
hc2 <- hclust(d.norm, method="average")
plot(hc2, hang=-4, ann=FALSE,  main="Average Linkage Dendrogram", xlab="Wines")
#ward's method
hc3 <- hclust(d.norm, method="ward.D")
plot(hc3, hang=-4, ann=FALSE,  main="Ward's Dendrogram", xlab="Wines")
#clustering via cutting dendrogram: should be in three groups
#ward's method
memb <- cutree(hc3, k=3)
memb
#creating heatmap
row.names(train) <- paste(memb, ": ", row.names(train), sep="")
heatmap(as.matrix(train), Colv = NA, hclustfun = hclust)
#run kmeans algorithm
set.seed(2)
#look at set seed documentation
km <- kmeans(wine2, 3)
summary(km)
#cluster membership
km$cluster
#centroids
km$centers
#most differing values = chlorides, free sulfur dioxide, and residual sugar
#within-cluster sum of squares
km$withinss
#cluster size
km$size
#labeling x axis for features
par(mar=c(1,1,1,1))
axis(1, at=c(1:11), labels=names(wine2))
#sum of within-cluster sum of squares
km$tot.withinss
#between cluster distance
km$betweenss
km

###
#MODEL 5
#Regression: Multivariate Linear Regression for Wines to Predict Alcohol Level
#cannot occur on ID or Quality
wine2 <- wine1[,c(2:12)]
train.index <- sample(c(1:dim(wine2)[1]), dim(wine2)[1]*0.8)  
train <- wine2[train.index, ]
valid <- wine2[-train.index, ]

names(wine1)
class(wine1$ID)
#int
class(wine1$fixed.acidity)
class(wine1$volatile.acidity)
class(wine1$citric.acid)
class(wine1$residual.sugar)
class(wine1$chlorides)
class(wine1$free.sulfur.dioxide)
class(wine1$total.sulfur.dioxide)
class(wine1$density)
class(wine1$pH)
class(wine1$sulphates)
class(wine1$alcohol)
class(wine1$quality)
#int

names(train)
cor(wine1$alcohol, y = wine1[, c(2:11)], method = c("pearson"))  
lm_wine <- lm(alcohol~residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide,
              data=train)  
lm_wine
summary(lm_wine)
#all factors are significant, so do not need to improve model with different parameters
#coefficients
c_lm_wine <- coef(lm_wine)
c_lm_wine
round(data.frame(summary(lm_wine)$coefficients, odds=exp(coef(lm_wine))),5)

#MSE Value = 0.956
mean(lm_wine$residuals^2)

#Validation & Prediction
pred <- predict(lm_wine, valid, se.fit=TRUE, interval="prediction")
pred
summary(pred)
#standard error of predicted mean = 148
data.frame( R2 = R2(pred$fit, valid$alcohol),
            RMSE = RMSE(pred$fit, valid$alcohol),
            MAE = MAE(pred$fit, valid$alcohol))

#####
#MODEL 6
#Classification: Classification Decision Tree for Wine Quality
names(wine1)
wine1_slice <- wine1[,c(2:13)] 
train.index <- sample(c(1:dim(wine1_slice)[1]), dim(wine1_slice)[1]*0.8)  
train4 <- wine1_slice[train.index, ]
valid4 <- wine1_slice[-train.index, ]
heatmap(as.matrix(wine1_slice), Colv = NA, hclustfun = hclust)
cor(x=wine1_slice$quality, y=wine1_slice)
#Decision Tree Model
TreeModel <- rpart(quality~.,
                   data=train4,
                   method = "class")
TreeModel
summary(TreeModel)

#Prediction
model.pred.train <- predict(TreeModel, train4, type = "class")
head(model.pred.train)

model.pred.train
#confusion matrix
confusionMatrix(model.pred.train, as.factor(train4$quality))
#Issues with this

#Validation
model.pred.valid <- predict(TreeModel, valid4, type = "class")
head(model.pred.train)
confusionMatrix(model.pred.valid, as.factor(valid4$quality))
printcp(TreeModel)
plotcp(TreeModel)

length(as.factor(wine1_slice$quality))

# Create a lift chart
pred$fit
length(wine1_slice$quality)
length(pred$fit)
gain<-gains(valid4$quality, as.numeric(model.pred.valid))
gain
