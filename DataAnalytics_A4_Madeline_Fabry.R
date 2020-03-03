queens <- read.csv("rollingsales_queens.csv")
queens
attach(queens)

#1a). Describe the type of patterns or trends you might 
#look for and how you plan to model them. 
#Describe any exploratory data analysis you performed. 
#Include plots and other descriptions. 
#Min. 2-3 sentences (2%)
names(queens)
str(queens)
summary(queens)
head(queens)

#Numeric columns
#Changing formating of LAND.SQUARE.FEET and GROSS.SQUARE.FEET to be numeric
queens$RESIDENTIAL.UNITS <- as.numeric(gsub(",","",queens$RESIDENTIAL.UNITS))
summary(queens$RESIDENTIAL.UNITS)
hist(queens$RESIDENTIAL.UNITS)
boxplot(queens$RESIDENTIAL.UNITS)

queens$COMMERCIAL.UNITS <- as.numeric(gsub(",","",queens$COMMERCIAL.UNITS))
summary(queens$COMMERCIAL.UNITS)
boxplot(queens$COMMERCIAL.UNITS)

queens$TOTAL.UNITS <- as.numeric(gsub(",","",queens$TOTAL.UNITS))
summary(queens$TOTAL.UNITS)
boxplot(queens$TOTAL.UNITS)

queens$LAND.SQUARE.FEET <- as.numeric(gsub(",","",queens$LAND.SQUARE.FEET))
summary(queens$LAND.SQUARE.FEET)
boxplot(queens$LAND.SQUARE.FEET)

queens$GROSS.SQUARE.FEET <- as.numeric(gsub(",","",queens$GROSS.SQUARE.FEET))
summary(queens$GROSS.SQUARE.FEET)
boxplot(queens$GROSS.SQUARE.FEET)
qqnorm(queens$GROSS.SQUARE.FEET); qqline(queens$GROSS.SQUARE.FEET)

summary(queens$YEAR.BUILT)
summary(queens$TAX.CLASS.AT.TIME.OF.SALE)

queens$SALE.PRICE <- as.numeric(gsub('[$,]', '', queens$SALE.PRICE))
summary(queens$SALE.PRICE)
boxplot(queens$SALE.PRICE)
plot(ecdf(queens$SALE.PRICE))
qqnorm(queens$SALE.PRICE); qqline(queens$SALE.PRICE)

install.packages("ggplot2", dependencies=TRUE)
library(ggplot2)
ggplot(data = queens) + geom_bar(mapping = aes(x = TAX.CLASS.AT.PRESENT))
ggplot(data = queens) + geom_bar(mapping = aes(x = NEIGHBORHOOD))
ggplot(data = queens) + geom_bar(mapping = aes(x = BUILDING.CLASS.CATEGORY))
ggplot(data = queens) + geom_bar(mapping = aes(x = EASE.MENT))
ggplot(data = queens) + geom_bar(mapping = aes(x =BUILDING.CLASS.AT.PRESENT))

library(dplyr)
copy <- select(queens,-c(SALE.DATE))
cor(copy[sapply(copy, function(x) !is.factor(x))])

#1b). 
mm <- lm(SALE.PRICE~ZIP.CODE + RESIDENTIAL.UNITS + COMMERCIAL.UNITS + TOTAL.UNITS + LAND.SQUARE.FEET + GROSS.SQUARE.FEET + YEAR.BUILT, data=queens)
mm
summary(mm)
#multivariate regression was performed of numeric columns to output SALE.PRICE
#The predictability of ZIP.CODE, LAND.SQUARE.FEET, and GROSS.SQUARE.FEET are significant at an alpha value of 0.1.
#The adjusted r-squared value is 0.3012, meaning that the regression model explains 30.12% of the variability of sale price
#Removing zero values was considered, but that would result in removal of too much data. 
#The regression model has a p-value < 2.2e-16, so the null hypothesis is rejected that the columns in the model are unrelated to sale price. 


#2a). 
set.seed(100)  
trainingRowIndex <- sample(1:nrow(queens), 0.8*nrow(queens))  # row indices for training data
trainingData <- queens[trainingRowIndex, ]  # model training data
testData  <- queens[-trainingRowIndex, ]  
mm2 <- lm(SALE.PRICE~ZIP.CODE + RESIDENTIAL.UNITS + COMMERCIAL.UNITS + TOTAL.UNITS + LAND.SQUARE.FEET + GROSS.SQUARE.FEET + YEAR.BUILT, data=trainingData)
distPred <- predict(mm2, testData)
summary(mm2)
distPred
plot(distPred, testData$SALE.PRICE)
#A 80-20 test-train split was performed, in which a linear model was created to predict the SALE.PRICE of the test values. 
#The validity and significance of the linear model, and a plot comparing predictions to actual test SALE.PRICE values.
#The model can account for predicting 39.05% of the variability in the test data set's sale price. 

#2b). 
ks.test(testData$SALE.PRICE, trainingData$SALE.PRICE)
#A Kolmogorov-Smirnov test was performed to test the distribution of the sale price between the train and test sets. 
#The p-value =0.01966<0.05, so the null hypothesis is rejected. The train and test set sale prices have similar 
#distributions, so the test-train split is valid. 

#2c). 
#I have low confidence in the results, given how many 0-value entries exist in the dataset. 
#As a rule-of-thumb, a regression model should have an R-squared value >0.7.


#3. 
#A more accurate model could be created by converting categorical variables to one-hot encoding, 
#though the amount of categorical factors would require a time-intensive data transformation. 
#While it logically makes sense to predict sale price of houses, perhaps predicting other parameter
#would have resulted in a more accurate model. 
#The multivariate regression model was statistically significant. The residual standard error
#was high. The multiple r-squared and adjusted r-squared values are low. The model had high error and low
#predictability power. #The model could have been less error-prone if the parameters were more normally
#distributed. 


