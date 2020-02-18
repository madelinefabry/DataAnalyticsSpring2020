setwd("~/Documents/Grad School Sem2/Data Analytics")

#Lab2a
#Remember head(), tail(), summary()
EPI_data <- read.csv("/Users/madeline/Documents/Grad School Sem2/Data Analytics/2010EPI_data.csv")
attach(EPI_data)

EPI <- array(EPI_data$EPI)
tf <- is.na(EPI) #records True values if the value is NA
EPI <- EPI[!tf]

DALY <- array(EPI_data$DALY)
dtf <- is.na(DALY) #records True values if the value is NA
DALY <- DALY[!dtf]


#Measures of Central Tendency
#EPI mean
mean(EPI)
#EPI mode
mode <- function(v){ 
  ta = table(v)
  tam = max(ta)
  is.numeric(v)
  mod = as.numeric(names(ta)[ta == tam])
  mod = names(ta)[ta == tam]
  return(mod)
}
mode(EPI)
#EPI median
median(EPI)

#DALY mean
mean(DALY)
#DALY mode
mode(DALY)
#DALY median
median(DALY)

#Generate the Histogram for EPI and DALY variables
#EPI Histogram
hist(EPI)
#DALY Histogram
hist(DALY)

install.packages("dplyr")
library(dplyr)
head(EPI_data)


#Dplyr exercises
#sample_n, 5 random data points for EPI, DALY
sample_n(filter(EPI_data, !is.na(EPI)),5)$EPI
sample_n(filter(EPI_data, !is.na(DALY)),5)$DALY

#sample_frac() function, 10% random data points for EPI, DALY
sample_frac(filter(EPI_data, !is.na(EPI)),.1)$EPI
sample_frac(filter(EPI_data, !is.na(DALY)),.1)$DALY

#arrange() and desc() to arrange values in descending order in EPI and DALY
#assign them to new_decs_EPI and new_decs_DALY
new_decs_EPI <- EPI_data %>% filter(!is.na(EPI)) %>% arrange(desc(EPI))
new_decs_DALY <- EPI_data %>% filter(!is.na(DALY)) %>% arrange(desc(DALY))

#mutate() function to create new columns double_EPI and double_DALY
#multiply values by 2 
EPI_data %>% filter(!is.na(EPI)) %>% mutate(double_EPI = EPI*2)
EPI_data %>% filter(!is.na(DALY)) %>% mutate(double_DALY = DALY*2)

#summarise() function along with mean() function to find
#mean for EPI and DALY
summarise(EPI_data,
          epi = mean(EPI, na.rm=TRUE),
          daly = mean(EPI, na.rm=TRUE))

boxplot(EPI_data$ENVHEALTH, EPI_data$ECOSYSTEM)
qqplot(EPI_data$ENVHEALTH, EPI_data$ECOSYSTEM)

#Exercise 2b
Europe <- filter(EPI_data, EPI_data$EPI_regions == 'Europe')
colnames(Europe)

filtered(EPI_data)
ENVHEALTH <- filter(EPI_data$ENVHEALTH, !is.na(ENVHEALTH))
lm(EPI_data$EPI ~. )

#Linear and Least-Squares
EPI_data <- read.csv("/Users/madeline/Documents/Grad School Sem2/Data Analytics/2010EPI_data.csv")
attach(EPI_data)
par(mar=c(1,1,1,1))
boxplot(EPI_data$ENVHEALTH, EPI_data$DALY, EPI_data$AIR_H, EPI_data$WATER_H)
lmENVH <- lm(EPI_data$ENVHEALTH ~ EPI_data$DALY + EPI_data$AIR_H + EPI_data$WATER_H)
lmENVH
summary(lmENVH)
cENVH <- coef(lmENVH)

#Predict
DALYNEW <- c(seq(5,95,5))
AIR_HNEW <- c(seq(5,95,5))
WATER_HNEW <- c(seq(5,95,5))
NEW < - data.frame(DALYNEW, AIR_HNEW, WATER_HNEW)
pENV <- suppressWarnings(predict(lmENVH, data=NEW, interval="prediction"))
cENV <- predict(lmENVH, data=NEW, interval="confidence")
#repeat for AIR_E
lmAIR_E <- lm(EPI_data$AIR_E ~ EPI_data$DALY + EPI_data$AIR_H + EPI_data$WATER_H)
lmAIR_E
summary(lmAIR_E)
cAIR_E <- coef(lmAIR_E)
pAIR_E <- suppressWarnings(predict(lmAIR_E, data=NEW, interval="prediction"))
cAIR_E <- predict(lmAIR_E, data=NEW, interval="confidence")

#repeat for CLIMATE
lmCLIMATE <- lm(EPI_data$CLIMATE ~ EPI_data$DALY + EPI_data$AIR_H + EPI_data$WATER_H)
lmCLIMATE
summary(lmCLIMATE)
cAIR_E <- coef(lmCLIMATE)
pAIR_E <- suppressWarnings(predict(lmCLIMATE, data=NEW, interval="prediction"))
cAIR_E <- predict(lmCLIMATE, data=NEW, interval="confidence")


#LAB2 PART 2

#Exercise 1: Regression 
dmR <- read.csv("/Users/madeline/Documents/Grad School Sem2/Data Analytics/dataset_multipleRegression.csv")
attach(dmR)
lmROLL <- lm(ROLL ~ UNEM + HGRAD, data=dmR)
new_Roll <- data.frame(UNEM=7.0, HGRAD=90000)
pROLL <- suppressWarnings(predict(lmROLL, newdata=new_Roll, interval="prediction"))
pROLL

lmROLL2 <- lm(ROLL ~ UNEM + HGRAD + INC)
new_Roll2 <- data.frame(UNEM=7.0, HGRAD=90000, INC=25000)
pROLL2 <- suppressWarnings(predict(lmROLL2, newdata=new_Roll2, interval="prediction"))
pROLL2

#Exercise 2: Classification
abalone <- read.csv("/Users/madeline/Documents/Grad School Sem2/Data Analytics/abalone.csv")
attach(abalone)
rows <- c("Length", "Diameter", "Height", 'Whole.weight', "Shucked.weight",
          "Viscera.weight", "Shell.weight", "Rings")
aba <- abalone[rows]
normalize <- function(x){
  return ((x-min(x)) / (max(x) - min(x)))
}
aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))
ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]
library(class)
classif <- knn(train=KNNtrain[1:7], test=KNNtest[1:7], cl=KNNtrain$Rings, k=55)
classif
attributes(.Last.value)


#Exercise 3: Clustering
attach(iris)
iris
rows <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
irisdf <- iris[rows]
irisdf
k.max <- 12
wss<- sapply(1:k.max,function(k){
  kmeans(irisdf,k,nstart = 20,iter.max = 1000)$tot.withinss})
wss
specieskm <- kmeans(irisdf, 3, iter.max=1000)
table(iris$Species,specieskm$cluster)

