#Final Project: NY Birth Rates Project
#Author: Maddie Fabry
setwd("~/Documents/Grad School Sem2/Data Analytics/")

#uploading libraries
install.packages("gains")
install.packages("naniar")
install.packages("usmap")
library(usmap)
library(naniar)
library(gains)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(dplyr)
library(fastDummies)

#Upload files
births <- read.csv("BirthRatesData.csv")
fips <- read.csv("NY_Municipalities_and_County_FIPS_codes.csv")
fips
births
#View(births)
attach(births)
fix(births)

#####
#DATA DESCRIPTION
head(births)
dim(births)
#Each row represents a zip code in NYS
names(births)
nrow(births)
ncol(births)
str(births)
summary(births)
#three features are not numeric: ZIP.Code, Teen.Birth.Rate.per.1000, Teen.Pregnancy.Rate.per.1000
#Qualitative data: County
#Categoricla data: Zip Code
#Quantitative data: 
#Discrete data: Infant.Deaths.2014.2016.per.1000.Live.Births, Total.Births.2014.2016
#Continuous data: low birth weight percent, out of wedlock percent, medicaid or self paid percent, late or no prenatal care percent

#Population sample of NY State
#Since data is a population, will not need to perform sampling

sapply(births, class)


####
#ANALYSIS 

#PART I) DISTRIBUTION EXPLORATION
par(mar=c(1,1,1,1))

#exploring factors: county
unique(births$County)
head(fips)
unique(fips$County.Name)
#61 Counties in NY State
summary(births$County)
length(unique(births$ZIP.Code))
#1450 ZIP Codes in NYS

#Convert features to numeric, except for County, Zip Code
sapply(births, class)

names(births)[names(births) == "Total.Births.2014.2016"] <- "Total.Births"
names(births)[names(births) == "Infant.Deaths.2014.2016.per.1000.Live.Births"] <- "Infant.Deaths"
names(births)[names(births) == "Infant.Deaths.Rate.per.1000.Live.Births"] <- "Infant.Deaths.Rate"
names(births)[names(births) == "Neonatal.Deaths.2014.2016.per.1000.Live.Births"] <- "Neonatal.Deaths"
names(births)[names(births) == "Neonatal.Dearths.Rate.per.1000.Live.Births"] <- "Neonatal.Deaths.Rate"
names(births)[names(births) == "Teen.Birth.Rate.per.1000"] <- "Teen.Birth.Rate"
names(births)[names(births) == "Teen.Pregnancy.Rate.per.1000"] <- "Teen.Pregnancy.Rate"
names(births)[names(births) == "Low.Birth.Rate.Perc"] <- "Low.Birth.Weight.Rate"
names(births)[names(births) == "Out.of.Wedlock.Perc"] <- "Out.of.Wedlock.Rate"
names(births)[names(births) == "Medicaid.or.Self.Pay.Perc"] <- "Medicaid.or.Self.Pay.Rate"
names(births)[names(births) == "Late.or.No.Prenatal.Care.Perc"] <- "Late.or.No.Prenatal.Care.Rate"
names(births)


#Make Total Births numeric and replace * with 0 
births$Total.Births <- as.numeric(births$Total.Births)
births[births$Teen.Birth.Rate =='*'] = 0
births$Teen.Birth.Rate <- as.numeric(births$Teen.Birth.Rate)

#Make Teen Pregnancy Rate per 1000 Numeric and replace * with 0 
births$Teen.Pregnancy.Rate <- as.numeric(births$Teen.Pregnancy.Rate)
births[births$Teen.Pregnancy.Rate =='*'] = 0

#Make Teen Birth Rate per 1000 Numeric and replace * with 0 
sapply(births,class)
summary(births$Teen.Birth.Rate)
summary(births$Teen.Pregnancy.Rate)

#Class, FiveNum, and Shapiro Tests
sapply(births[,-c(1,2)], class)
sapply(births[,-c(1,2)], fivenum)
sapply(births[,-c(1,2)], shapiro.test)

names(births)

#EDA, DISTRIBUTIONS
#exploring distribution: Total.Births
par(mar=c(1,1,1,1))
stem(births$Total.Births)
boxplot(births$Total.Births)
#no outliers
hist(births$Total.Births)
hist(births$Total.Births, seq(0., 700., 10.0), prob=TRUE)
#distribution = chi-square distribution or weilbull distribution
lines(density(births$Total.Births,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(births$Total.Births)
plot(ecdf(births$Total.Births, do.points=FALSE, verticals=TRUE))
par(pty="s")
qqnorm(births$Total.Births)
qqline(births$Total.Births)
x <- seq(0, 700, 10)
qqplot(qt(ppoints(250), df=5), x, xlab='Q-Q plot for t dist')
qqline(x)

#exploring distribution: Premature.Births.Perc 
par(mar=c(1,1,1,1))
stem(births$Premature.Birth.Perc)
boxplot(births$Premature.Birth.Perc)
#many outliers -> explore later
hist(births$Premature.Birth.Perc)
#positive skew
lines(density(births$Premature.Birth.Perc,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(births$Premature.Birth.Perc)
plot(ecdf(births$Premature.Birth.Perc), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(births$Premature.Birth.Perc)
qqline(births$Premature.Birth.Perc)
x <- seq(0, 40, 1)
qqplot(qt(ppoints(250), df=5), x, xlab='Q-Q plot for t dist')
qqline(x)

#exploring distribution: Low.Birth.Weight.Rate
par(mar=c(1,1,1,1))
stem(births$Low.Birth.Weight.Rate)
boxplot(births$Low.Birth.Weight.Rate)
#outliers 
hist(births$Low.Birth.Weight.Rate)
#positive skew
hist(births$Low.Birth.Weight.Rate, seq(-1., 35., 1.), prob=TRUE)
lines(density(births$Low.Birth.Weight.Rate,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(births$Low.Birth.Weight.Rate)
plot(ecdf(births$Low.Birth.Weight.Rate), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(births$Low.Birth.Weight.Rate)
qqline(births$Low.Birth.Weight.Rate)
x <- seq(0, 35, 1)
qqplot(qt(ppoints(250), df=5), x, xlab='Q-Q plot for t dist')
qqline(x)

#exploring distribution: Out.of.Wedlock.Rate
par(mar=c(1,1,1,1))
stem(births$Out.of.Wedlock.Rate)
boxplot(births$Out.of.Wedlock.Rate)
#one outlier
hist(births$Out.of.Wedlock.Rate, seq(0., 100, 4), prob=TRUE)
lines(density(births$Out.of.Wedlock.Rate,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(births$Out.of.Wedlock.Rate)
plot(ecdf(births$Out.of.Wedlock.Rate), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(births$Out.of.Wedlock.Rate)
qqline(births$Out.of.Wedlock.Rate)
x <- seq(0, 100, 4)
qqplot(qt(ppoints(250), df=5), x, xlab='Q-Q plot for t dist')
qqline(x)

#exploring distribution: X..Medicaid.or.Self.Pay
par(mar=c(1,1,1,1))
stem(births$Medicaid.or.Self.Pay.Rate)
boxplot(births$Medicaid.or.Self.Pay.Rate)
hist(births$Medicaid.or.Self.Pay.Rate)
hist(births$Medicaid.or.Self.Pay.Rate, seq(0., 100., 1.0), prob=TRUE)
lines(density(births$Medicaid.or.Self.Pay.Rate,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(births$Medicaid.or.Self.Pay.Rate)
plot(ecdf(births$Medicaid.or.Self.Pay.Rate), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(births$Medicaid.or.Self.Pay.Rate)
qqline(births$Medicaid.or.Self.Pay.Rate)
x <- seq(0, 700, 10)
qqplot(qt(ppoints(250), df=5), x, xlab='Q-Q plot for t dist')
qqline(x)

#exploring distribution: Late.or.No.Prenatal.Care.Perc
par(mar=c(1,1,1,1))
stem(births$Late.or.No.Prenatal.Care.Rate)
boxplot(births$Late.or.No.Prenatal.Care.Rate)
#outilers
hist(births$Late.or.No.Prenatal.Care.Rate)
#positively skewed
hist(births$Late.or.No.Prenatal.Care.Rate, seq(0., 375., 25), prob=TRUE)
lines(density(births$Late.or.No.Prenatal.Care.Rate,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(births$Late.or.No.Prenatal.Care.Rate)
plot(ecdf(births$Late.or.No.Prenatal.Care.Rate), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(births$Late.or.No.Prenatal.Care.Rate)
qqline(births$Late.or.No.Prenatal.Care.Rate)
x <- seq(0, 375, 25)
qqplot(qt(ppoints(250), df=5), x, xlab='Q-Q plot for t dist')
qqline(x)

#exploring distribution: Infant.Deaths
par(mar=c(1,1,1,1))
stem(births$Infant.Deaths)
boxplot(births$Infant.Deaths)
#outliers -> make this the output parameter
hist(births$Infant.Deaths)
#positively skewed
hist(births$Infant.Deaths, seq(0., 375., 25), prob=TRUE)
lines(density(births$Infant.Deaths,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(births$Infant.Deaths)
plot(ecdf(births$Infant.Deaths), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(births$Infant.Deaths)
qqline(births$Infant.Deaths)
x <- seq(0, 375, 5)
qqplot(qt(ppoints(250), df=5), x, xlab='Q-Q plot for t dist')
qqline(x)

#exploring distribution: Infant.Deaths.Rate.per.1000.Live.Births  
par(mar=c(1,1,1,1))
stem(births$Infant.Deaths.Rate)
boxplot(births$Infant.Deaths.Rate)
#outliers
hist(births$Infant.Deaths.Rate)
#positive skew 
hist(births$Infant.Deaths.Rate, seq(0., 250., 25), prob=TRUE)
lines(density(births$Infant.Deaths.Rate,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(births$Infant.Deaths.Rate)
plot(ecdf(births$Infant.Deaths.Rate), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(births$Infant.Deaths.Rate)
qqline(births$Infant.Deaths.Rate)
x <- seq(0, 250, 10)
qqplot(qt(ppoints(250), df=5), x, xlab='Q-Q plot for t dist')
qqline(x)

#exploring distribution: Neonatal.Deaths.2014.2016.per.1000.Deaths 
par(mar=c(1,1,1,1))
stem(births$Neonatal.Deaths)
boxplot(births$Neonatal.Deaths)
#outliers
hist(births$Neonatal.Deaths)
#positive skew
hist(births$Neonatal.Deaths, seq(0., 250, 25), prob=TRUE)
lines(density(births$Neonatal.Deaths,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(births$Neonatal.Deaths)
plot(ecdf(births$Neonatal.Deaths), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(births$Neonatal.Deaths)
qqline(births$Neonatal.Deaths)
x <- seq(0, 250, 10)
qqplot(qt(ppoints(250), df=5), x, xlab='Q-Q plot for t dist')
qqline(x)

#exploring distribution: Neonatal.Deaths.Rate.per.1000.Deaths  
par(mar=c(1,1,1,1))
stem(births$Neonatal.Deaths.Rate)
boxplot(births$Neonatal.Deaths.Rate)
#outliers
hist(births$Neonatal.Deaths.Rate)
#positive skew
hist(births$Neonatal.Deaths.Rate, seq(0., 250, 10), prob=TRUE)
lines(density(births$Neonatal.Deaths.Rate,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(births$Neonatal.Deaths.Rate)
plot(ecdf(births$Neonatal.Deaths.Rate), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(births$Neonatal.Deaths.Rate)
qqline(births$Neonatal.Deaths.Rate)
x <- seq(0, 250, 10)
qqplot(qt(ppoints(250), df=5), x, xlab='Q-Q plot for t dist')
qqline(x)

#exploring distribution: Teen.Birth.Rate.per.1000  
par(mar=c(1,1,1,1))
stem(births$Teen.Birth.Rate)
boxplot(births$Teen.Birth.Rate)
hist(births$Teen.Birth.Rate)
hist(births$Teen.Birth.Rate, seq(0., 375., 25), prob=TRUE)
lines(density(births$Teen.Birth.Rate,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(births$Teen.Birth.Rate)
plot(ecdf(births$Teen.Birth.Rate), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(births$Teen.Birth.Rate)
qqline(births$Teen.Birth.Rate)
x <- seq(0, 375, 5)
qqplot(qt(ppoints(250), df=5), x, xlab='Q-Q plot for t dist')
qqline(x)

#exploring distribution: Teen.Pregnancy.Rate.per.1000 
par(mar=c(1,1,1,1))
stem(births$Teen.Pregnancy.Rate)
boxplot(births$Teen.Pregnancy.Rate)
hist(births$Teen.Pregnancy.Rate)
hist(births$Teen.Pregnancy.Rate, seq(0., 500., 25), prob=TRUE)
lines(density(births$Teen.Pregnancy.Rate,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(births$Teen.Pregnancy.Rate)
plot(ecdf(births$Teen.Pregnancy.Rate), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(births$Teen.Pregnancy.Rate)
qqline(births$Teen.Pregnancy.Rate)
x <- seq(0, 500, 10)
qqplot(qt(ppoints(250), df=5), x, xlab='Q-Q plot for t dist')
qqline(x)

#Data Cleaning: 

#Correlations
#Heatmap
par(mar=c(1,1,1,1))
dev.off()
par(mar=c(1,1,1,1))
heatmap_df <- births[,-c(1,2)]
heatmap_df <- scale(heatmap_df, scale=TRUE)
heatmap(as.matrix(heatmap_df))

sapply(births, class)
#Correlation Matrix
res <- cor(births[,-c(1,2)], method="pearson")
res
res[res < 0.5 | res ==1] <- ""
res
max(res)
names(births)

#Correlated Features
#Neonatal and Infant Deaths (0.968)
#Neonatal and Infant Deaths Rates (0.927)
#Out of Wedlock and Medicaid (.742) 
#Premature and Low Birth Weight (0.537)

#Selected Numeric Features
#[3] "Total.Births"                 
#[5] "Low.Birth.Weight.Perc"        
#[7] "Medicaid.or.Self.Pay.Perc"    
#[8] "Late.or.No.Prenatal.Care.Perc"
#[9] "Infant.Deaths"                
#[10] "Infant.Deaths.Rate"           
#[13] "Teen.Birth.Rate"              
#[14] "Teen.Pregnancy.Rate" 

births1 <- births[,c(3,5,7, 9, 10, 13:14)]
names(births1)
births1 <- scale(births1, scale=TRUE)
births1 <- data.frame(births1)
class(births1)
#Source of Error: 
#Confounding Factor of Population
#No access to Population for time frame (2014-2016)
#Total Births will likely reflect population




#Time to Plot NY Population
install.packages("devtools")
require("devtools")
#install_github(“UrbanInstitute/urbnmapr”)


names(births)
mapping_df <- births[,c(1,3)]
names(mapping_df)
help(plot_usmap)
mapping_df
usmap::plot_usmap(data=mapping_df,
                  values="Total.Births",
                  "counties",
                  include = c("NY"))

library(usmap)
library(ggplot2)

library(data.frame)
names(fips)[names(fips) == "County.Name"] <- "County"
names(fips)[names(fips) == "County.FIPS"] <- "fips"


birth.dt <- data.frame(births)
birth.dt[,list(Infant.Deaths=sum(Infant.Deaths)), by="County"]

fips1 <- merge(x=birth.dt, y=fips, by.x='County')
names(fips1)
fips1 <- fips1[,c(20,3)]

usmap::plot_usmap(data=fips1,
                  values=Total.Births.2014.2016,
                  "counties",
                  include = c("NY"))

usmap::plot_usmap("counties",
                  fill="lightblue",
                  include = c("NY"))

names(births)


#KNN clustering analysis 
#Clustering
# Partitioning into training (80%) and validation (20%)
train.index <- sample(c(1:dim(births1)[1]), dim(births1)[1]*0.8)  
train <- births1[train.index, ]
valid <- births1[-train.index, ]

d.norm <- dist(train, method="euclidean")
max(d.norm)
par(mar=c(1,1,1,1))

#hierarchical agglomerative clustering
#Dendogram for Single Linkage
#hclust() methods: "ward.D", "single", "complete", "average", "median", or "centroid"
#ward's method
hc <- hclust(d.norm, method="ward.D")
plot(hc, hang=-4, ann=FALSE,  main="Ward's Dendrogram", xlab="Wines")
#clustering via cutting dendrogram: should be in three groups

# Determine number of clusters
wss <- (nrow(births1)-1)*sum(apply(births1,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(births1,
                                     centers=i)$withinss)
plot(1:15, wss, type = "b",
     xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")
#Clusters from graph: eight clusters

#ward's method
memb <- cutree(hc, k=8)
memb
class(memb)

#creating heatmap
row.names(train) <- paste(memb, ": ", row.names(train), sep="")

####
#Cluster Model Application
#run kmeans algorithm
set.seed(2)
names(births)
unscaled <- births[,c(3,5,7:8, 10, 13:14)]
names(unscaled)
km_test <- kmeans(unscaled,8)

births1

km <- kmeans(births1, 8)
km

summary(km)
#cluster membership
km$cluster
#centroids
km$centers
#most differing values = chlorides, free sulfur dioxide, and residual sugar
#within-cluster sum of squares
km$withinss
km_test$withinss
#cluster size
km$size
#labeling x axis for features
par(mar=c(1,1,1,1))
axis(1, at=c(1:7), labels=names(births1))
#sum of within-cluster sum of squares
km$tot.withinss
#between cluster distance
km$betweenss
#km

min(km$centers)

#plot profile plot of centroids
plot(c(0), xaxt='n', ylab="", type="l",
     ylim=c(min(km$centers), max(km$centers)),
     xlim=c(-2,7), xlab="Grades")
#labeling x axis for features
axis(1, at=c(1:7), labels=names(births1))
#plotting centroids
for (i in c(1:7))
  points(km$centers[i,], lty=i, lwd=2, col=ifelse(i%in% c(1,7), "black", "dark grey"))
#name clusters
text(x=-1, y=km$centers[,1], labels=paste("Cluster", c(1:7)))

#Euclidean distance between final cluster centroids
sum(dist(km$centers))

births2 <- births[,c(3, 5, 7:10, 13:14)]
class(births2)
help(trainControl)
trControl <- trainControl(method  = "cv",
                          number  = 10)
names(births2)
fit <- train(Infant.Deaths ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:8),
             trControl  = trControl,
             metric     = "RMSE",
             data       = births2)
fit

help(train)

km <- kmeans(births1, 4)
km

summary(km)
#cluster membership
km$cluster
#centroids
km$centers
#most differing values = chlorides, free sulfur dioxide, and residual sugar
#within-cluster sum of squares
km$withinss
km_test$withinss
#cluster size
km$size
#labeling x axis for features
par(mar=c(1,1,1,1))
axis(1, at=c(1:7), labels=names(births1))
#sum of within-cluster sum of squares
km$tot.withinss
#between cluster distance
km$betweenss
#km

min(km$centers)

#plot profile plot of centroids
plot(c(0), xaxt='n', ylab="", type="l",
     ylim=c(min(km$centers), max(km$centers)),
     xlim=c(-2,7), xlab="Predictors")
#labeling x axis for features
axis(1, at=c(1:7), labels=names(births1))
#plotting centroids
for (i in c(1:4))
  points(km$centers[i,], lty=i, lwd=2, col=(ifelse(i%in% c(1,2), "black", "dark grey")))
#name clusters
text(x=-1, y=km$centers[,1], labels=paste("Cluster", c(1:4)))

#Euclidean distance between final cluster centroids
sum(dist(km$centers))


#MODEL 2
births1 <- births1[,c(-5)]
names(births1)
train.index <- sample(c(1:dim(births1)[1]), dim(births1)[1]*0.8)  
train <- births1[train.index, ]
valid <- births1[-train.index, ]


set.seed(00)
library(randomForest)
library(caret)
help(randomForest)
names(train)

rf <- randomForestRegressor(Infant.Deaths ~ ., 
                   train,
                   predicted=test,
                   type="regression",
                   keep.forest=TRUE,
                   importance=TRUE)
summary(rf)

sum(rf$mse)
mean(rf$rsq)
rf$ntree
rf$mtry
rf$terms
rf$importance

names(train)
names(valid)
rf_pred <- predict(rf, newdata=valid, keep.forest=TRUE)
data.frame( R2 = R2(rf_pred, valid$Infant.Deaths),
            RMSE = RMSE(rf_pred, valid$Infant.Deaths),
            MAE = MAE(rf_pred, valid$Infant.Deaths))

train.control <- trainControl(method = "LOOCV")
rf <- randomForest(Infant.Deaths ~ ., 
                    train,
                    predicted=test,
                    type="regression",
                    keep.forest=TRUE,
                    importance=TRUE,
                    random_state=42,
                    trContol = train.control)
rf$importance
summary(rf)

sum(rf$mse)
mean(rf$rsq)
rf$ntree
rf$mtry
rf$terms

importance <- varImp(rf, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

names(train)
names(valid)
rf_pred <- predict(rf, newdata=valid, keep.forest=TRUE)
data.frame( R2 = R2(rf_pred, valid$Infant.Deaths),
            RMSE = RMSE(rf_pred, valid$Infant.Deaths),
            MAE = MAE(rf_pred, valid$Infant.Deaths))

#Variable Importance

rfImp <- varImp(rf, scale = FALSE)
rfImp
plot(names(train),rfImp)

roc_imp <- filterVarImp(x = xtrain, y = ytrain)
head(roc_imp)

imp <- importance(rf)
varImp <- data.frame(Variables = row.names(imp))

importance <- varImp(rf, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


ggplot(rankImp, aes(reorder(Variables, imp[,1]), imp[,1], 
                    fill = imp[,1])) +
  geom_bar(stat='identity') + 
  geom_text(aes(Variables, 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'yellow') +
  labs(x = 'Variables', y='Importance') +
  coord_flip()

