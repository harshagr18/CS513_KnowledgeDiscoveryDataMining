# Course      : CS 513
# First Name  : Harsh
# Last Name   : Agrawal
# CWID        : 10475285

rm(list=ls())
library(class)
library(rpart)

setwd("C:/Users/Asus/Desktop/Github/MSCS/CS-513/MidTerm")

covidData <- read.csv('COVID19_v4.csv',header=TRUE, sep=",")
covidData <- na.omit(covidData)
covidData <- covidData[-1]
summary(covidData)

covidData$MonthAtHospital[covidData$MonthAtHospital>=6]<-c("6 or more months")
covidData$MonthAtHospital[covidData$MonthAtHospital<6]<-c("less than 6 months")

covidData$Age[covidData$Age<35]<-c("less than 35")
covidData$Age[covidData$Age>=35&covidData$Age<=50]<-c("35 to 50")
covidData$Age[covidData$Age>51]<-c("51 or over")

train_index <- sample(nrow(covidData),as.integer(.70*nrow(covidData)))
train_data<-covidData[train_index,]
test_data<-covidData[-train_index,]
View(covidData)

CART_class <- rpart(Infected ~ ., data = train_data)
prediction <- predict(CART_class, test_data, type="class")


conf_matrix <- table(prediction,test_data$Infected)
print(conf_matrix)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)

rm(list=ls())
