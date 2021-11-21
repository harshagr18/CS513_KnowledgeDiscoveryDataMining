# Course      : CS 513
# First Name  : Harsh
# Last Name   : Agrawal
# CWID        : 10475285

rm(list=ls())
library(class)

setwd("C:/Users/Asus/Desktop/Github/MSCS/CS-513/MidTerm")

covidData <- read.csv('COVID19_v4.csv',header=TRUE, sep=",")
covidData <- na.omit(covidData)
covidData <- covidData[-4]
covidData <- covidData[-1]
summary(covidData)

trainingData = covidData[-5]
train_index <- sample(nrow(trainingData),as.integer(.70*nrow(trainingData)))
train_data<-trainingData[train_index,]
test_data<-trainingData[-train_index,]
View(trainingData)

trainTarget = covidData["Infected"]
trainTarget = trainTarget[train_index,]
testTarget = covidData["Infected"]
testTarget = testTarget[-train_index,]

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

clf <- knn(train_data,test_data,cl=trainTarget,k=3)
conf_matrix <- table(clf, testTarget)
print(conf_matrix)
accuracy(conf_matrix)