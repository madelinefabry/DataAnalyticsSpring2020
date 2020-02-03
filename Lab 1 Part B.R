multivariate <- read.csv("/Users/madeline/Documents/Grad School Sem2/Data Analytics/multivariate.csv")
setwd("/Users/madeline/Documents/Grad School Sem2/Data Analytics/")
attach(multivariate)
View(multivariate)
mm <- lm(Homeowners~Immigrants)
mm

#Multivariate Regression
head(multivariate)
attach(multivariate)
help(lm)
summary(mm)$cpef #The output above shows the estimate of the regression beta coefficients (column estimate)
#and their significance levels (column PR(>|t|))
#The intercept is 67382 and the coefficient of Immigrants variable is -2498
#Estimated regression equation:
#Homeowners = 67382 - 2498*Immigrants
plot(Homeowners~Immigrants)
help(abline)
abline(mm)
abline(mm, col=2, lwd=3)
newImmigrantdata <- data.frame(Immigrant = c(0,20))
mm %>% predict(newImmigrantdata)

abline(mm)
abline(mm,col=3, lwd=3) #line color = green, line width = 3
attributes(mm)
mm$coefficients

#Creating Plots
#Chapter 2 -- R Graphics Cookbook
plot(mtcars$wt, mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt, mtcars$mpg)
qplot(wt,mpg,data=mtcars)
ggplot(mtcars, aes(x=wt, y=mpg))+geom_point()
plot(pressure$temperature, pressure$pressure, type="l")
points(pressure$temperature, pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col="red")
points(pressure$temperature, pressure$pressure/2, col="blue")
library(ggplot2)
qplot(pressure$temperature, pressure$pressure, geom="line")
qplot(temperature, pressure, data=pressure, geom="line")
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line() + geom_point()
ggplot(pressure, aex(x=temperature, y=pressure)) + geom_lines() + geom_point()

#Creating Bar graphs
barplot(BOD$demand, names.arg=BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl)) #generate a table of counts
qplot(mtcars$cyl)
qlplot(factor(mtcars$cyl))

#Bar graph of counts 
qplot(factor(cyl), data=mtcars)
ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()

#Creating Histogram
#View the distribution of one-dimensional data with a histogram
hist(mtcars$mpg)
hist(mtcars$mpg, breaks=10) #specify approximate number of bins with breaks
hist(mtcars$mpg, breaks=5)
hist(mtcars$mpg, breaks=12)
qplot(mpg, data=mtcars, binwidth=4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth=4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth=5)

#Creating box-plot
plot(ToothGrowth$supp, ToothGrowth$len) #using plot() function and pass it a factor of x-values and a vector of y-values
#Formula Syntax
boxplot(len ~ supp, data = ToothGrowth) #if the two vectors are in the same dataframe, use formula syntax
#can combine two variables on the x-axis
#put interaction of two variables on x-axis
boxplot(len ~ supp + dose, data = ToothGrowth)
#with ggplot2 you can get the same results above
library(ggplot2)
qplot(ToothGrowth$supp, ToothGrowth$len, geom='boxplot')
#if the two vectors are in the same dataframe:
qplot(supp, len, data=ToothGrowth, geom="boxplot")
#equiv to:
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom="boxplot")
qplot(interaction(supp, dose), len, data=ToothGrowth, geom="boxplot")
ggplot(ToothGrowth, aes(x=interaction(supp,dose), y=len)) + geom_boxplot()





















