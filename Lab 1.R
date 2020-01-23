#Creating a dataframe
#Example: RPI Weather dataframe

days <- c('Mon', 'Tue', 'Wed', 'Thur', 'Fri', 'Sat', 'Sun') #days
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4) #Temp in F during the winter
snowed <- c('T', 'T', 'F', 'F', 'T','T', 'F') #Snowed on that day
help("data.frame")

RPI_Weather_Week <- data.frame(days, temp, snowed) #creating the dataframe using the dat.frame() function

RPI_Weather_Week
head(RPI_Weather_Week) #will only show first six rows of dataframe
#here we have only 7 rows in our dataframe

str(RPI_Weather_Week) #structure

summary(RPI_Weather_Week) #summary of the dataframe using the summary() function


RPI_Weather_Week[1,] #showing the 1st row and all of the columns
RPI_Weather_Week[,1] #showing the 1st column and all of the rows

RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5,c("days","temp")]
RPI_Weather_Week$temp
subset(RPI_Weather_Week, subset=snowed==TRUE) #this line will not work

sorted.snowed <- order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]


#RPI_Weather_Week[descending_snowed,]
dec.snow <- order(-RPI_Weather_Week$temp)
dec.snow
#Creating Dataframes
#Creating an empty dataframe
empty.DataFrame <- data.frame()
v1 <- 1:10
v1
letters #built in function of all letters in the English alphabet
v2 <- letters[1:10]
df <- data.frame(col.name.1 = v1, col.name.2 = v2) #assigning column names
df
#importing data and exporting data
#writing to a CSV file:
write.csv(df, file='saved_df1.csv')
df2 <- read.csv('saved_df1.csv')
df2

nrow(df)
ncol(df)
colnames(df)
rownames(df)
str(df)
summary(df)


EPI <- read.csv(file.choose(), header=T)
summary(EPI)
nrow(EPI)
ncol(EPI)
colnames(EPI)
rownames(EPI)

par(mar=c(1,1,1,1))
boxplot(EPI$DALY)
hist(EPI$Desert) #binary variable
hist(EPI$Landlock) #binary variable
boxplot(EPI$WATER_H) #negatively skewed
  
data()
help(data)

EPI_data <- read.csv("/Users/madeline/Documents/Grad School Sem2/Data Analytics/2010EPI_data.csv")
setwd("/Users/madeline/Documents/Grad School Sem2/Data Analytics/")
View(EPI_data)
attach(EPI_data)
fix$EPI(EPI)
tf <- is.na(EPI) #records True values if the value is NA
E <- EPI[!tf] #filters out NA values, new array


