# Course      : CS 513
# First Name  : Harsh
# Last Name   : Agrawal
# CWID        : 10475285


rm(list = ls())

# Load and summarize
setwd("C:/Users/Asus/Desktop/Github/MSCS/CS-513/MidTerm")
covidData <- read.csv(file = 'COVID19_v4.csv',header=TRUE, sep=",")
summary(covidData)
# From summary, missing values (NA) exist only in Age and MonthAtHospital

# Creating a function to calculate mode of the column

getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Replacing NA of the column with mode of the column
modeAge = getMode(covidData$Age)
covidData$Age[is.na(covidData$Age)] <- modeAge

modeMonthAtHospital = getMode(covidData$MonthAtHospital)
covidData$MonthAtHospital[is.na(covidData$MonthAtHospital)] <- modeMonthAtHospital

View(covidData)
  
#covidData["Age","Exposure","MonthAtHospital"]

# Scatter plot of Age, Exposure and MonthAtHospital
graph1Data = data.frame(covidData$Age,covidData$MaritalStatus,covidData$MonthAtHospital)
plot(graph1Data,main="Scatter plot of Age, Exposure and MonthAtHospital",col="red")

# box plots for columns: Age, and MonthAtHospital
graph2Data = data.frame(covidData$Age,covidData$MonthAtHospital)
boxplot(graph2Data,main="box plots for columns: Age, and MonthAtHospital",col="blue")

rm(list = ls())
