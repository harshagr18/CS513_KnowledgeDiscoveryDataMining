# Course      : CS 513
# First Name  : Harsh
# Last Name   : Agrawal
# CWID        : 10475285


rm(list = ls())

# Load and summarize
cancerData <- read.csv(file = 'breast-cancer-wisconsin.csv',na.string = "?")
summary(cancerData)
# From summary, missing values (NA) exist only in column F6

# Replacing NA of the column with mean of the column
sumF6 = sum(cancerData$F6, na.rm = TRUE)
cancerData$F6[is.na(cancerData$F6)] <- round(sumF6 / sum(!is.na(cancerData$F6)))

View(cancerData)

# Display the frequency table of Class vs F6
myTable <- table(cancerData$Class, cancerData$F6)
ftable(myTable)

try = select()

# Plot F1 to F6 taking pairs
plot(cancerData[2:7],main="F1 to F6 : scatter plot",col="red")

# Plot a histogram for F7 to F9
boxplot(cancerData[8:10],main="F7 to F9 : histogram box plot",col="blue")

# Remove all environment variables for reloading
rm(list = ls())

# Read the data set again
dataSet<-read.csv("breast-cancer-wisconsin.csv",header = TRUE,na.strings = "?")

# Removing all the columns with NA
dataSet <- na.omit(dataSet)

# Viewing the final data set
View(dataSet)
