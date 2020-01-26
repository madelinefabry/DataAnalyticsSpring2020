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
fix(EPI_data.EPI)
EPI_data$EPI
tf <- is.na(EPI) #records True values if the value is NA
E <- EPI[!tf] #filters out NA values, new array

#Exercise 1
summary(EPI_data$EPI)
fivenum(EPI_data$EPI, na.rm=TRUE)
stem(EPI_data$EPI)
hist(EPI_data$EPI)
hist(EPI_data$EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI_data$EPI, na.rm=TRUE, bw=1.)) #or try bw="SJ"
rug(EPI_data$EPI)
#Use help()

#Exercise 1: Fitting a distribution beyond histograms
plot(ecdf(EPI_data$EPI), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI)

x <- seq(30, 95, 1)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot for t dsn")
qqline(x)

summary(EPI_data$DALY)
fivenum(EPI_data$DALY, na.rm=TRUE)
stem(EPI_data$DALY)
hist(EPI_data$DALY)
hist(EPI_data$DALY, prob=TRUE)
lines(density(EPI_data$DALY, na.rm=TRUE, bw="SJ"))
rug(EPI_data$DALY)
plot(ecdf(EPI_data$DALY), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI_data$DALY)
qqline(EPI_data$DALY)
x <- seq(30, 100, 1)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot for t daly")
qqline(x)


summary(EPI_data$AIR_E)
fivenum(EPI_data$AIR_E, na.rm=TRUE)
stem(EPI_data$AIR_E)
hist(EPI_data$AIR_E)
hist(EPI_data$AIR_E, prob=TRUE)
lines(density(EPI_data$AIR_E, na.rm=TRUE, bw="SJ"))
rug(EPI_data$AIR_E)
plot(ecdf(EPI_data$AIR_E), do.points=FALSE, verticals=TRUE) #applies ecdf distribution
par(pty="s")
qqnorm(EPI_data$AIR_E)
qqline(EPI_data$AIR_E) #applies qq- distributions 
x <- seq(30, 100, 1)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot for t AIR")
qqline(x)

EPI <- EPI_data$EPI

#Comparing distributions
boxplot(EPI, DALY)
qqplot(EPI, DALY)

boxplot(EPI, ENVHEALTH, ECOSYSTEM, DALY, AIR_H, WATER_H, AIR_E, WATER_E, BIODIVERSITY)
qqplot(EPI, ENVHEALTH)
qqplot(EPI, ECOSYSTEM)
qqplot(EPI, AIR_H)
qqplot(EPI, WATER_H)
qqplot(EPI, AIR_E)
qqplot(EPI, WATER_E)
qqplot(EPI, BIODIVERSITY)
qqplot(ENVHEALTH, ECOSYSTEM)
qqplot(ENVHEALTH, DALY)
qqplot(ENVHEALTH, AIR_H)
qqplot(ENVHEALTH, WATER_H)
qqplot(ENVHEALTH, AIR_E)
qqplot(ENVHEALTH, WATER_E)
qqplot(ENVHEALTH, BIODIVERSITY) 
qqplot(ECOSYSTEM, DALY)
qqplot(ECOSYSTEM, AIR_H)
qqplot(ECOSYSTEM, WATER_H)
qqplot(ECOSYSTEM, AIR_E)
qqplot(ECOSYSTEM, WATER_E)
qqplot(ECOSYSTEM, BIODIVERSITY)
qqplot(DALY, AIR_H)
qqplot(DALY, WATER_H)
qqplot(DALY, AIR_E)
qqplot(DALY, WATER_E)
qqplot(DALY, BIODIVERSITY)
qqplot(AIR_H, WATER_H)
qqplot(AIR_H, AIR_E)
qqplot(AIR_H, WATER_E)
qqplot(AIR_H, BIODIVERSITY)
qqplot(WATER_H, AIR_E)
qqplot(WATER_H, WATER_E)
qqplot(WATER_H, BIODIVERSITY)
qqplot(AIR_E, WATER_E)
qqplot(AIR_E, BIODIVERSITY)
qqplot(WATER_E, BIODIVERSITY)

#Unsure whether to code anything to note 2010 and 2016 datasets

#Exercise 2: filtering (populations)
EPILand <- EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)

plot(ecdf(Eland), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(Eland)
qqline(Eland)

summary(Eland)
fivenum(Eland, na.rm=TRUE)
stem(Eland)
hist(Eland)
hist(Eland, prob=TRUE)
lines(density(Eland, na.rm=TRUE, bw="SJ"))
rug(Eland)
plot(ecdf(Eland), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(Eland)
qqline(Eland)
x <- seq(30, 100, 1)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot for Not Landlocked")
qqline(x)

#No_surface_water filter
EPISW <- EPI[!No_surface_water]
Esw <- EPISW[!is.na(EPISW)]
hist(Esw)
hist(Esw, seq(30., 95., 1.0), prob=TRUE)

#Desert filter
EPIDesert <- EPI[!Desert]
Edesert <- EPIDesert[!is.na(EPIDesert)]
hist(Edesert)
hist(Edesert, seq(30., 95., 1.0), prob=TRUE)

#High Population Density filter
EPIHPD <- EPI[!High_Population_Density]
Ehpd <- EPIHPD[!is.na(EPIHPD)]
hist(Ehpd)
hist(Ehpd, seq(30., 95., 1.0), prob=TRUE)

#Filter on EPI_regions or GEO_subregion
EPIR <- EPI[!EPI_regions=="East Asia and the Pacific "]
EPIr <- EPIR[!is.na(EPIR)]
hist(EPIr)
hist(EPIr, seq(30., 95., 1.0), prob=TRUE)
