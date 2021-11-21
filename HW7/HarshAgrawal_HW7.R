# Course      : CS 513
# First Name  : Harsh
# Last Name   : Agrawal
# CWID        : 10475285

rm(list=ls())

library(neuralnet)
library(NeuralNetTools)

setwd("C:/Users/Asus/Desktop/Github/MSCS/CS-513/HW7")

db = read.csv('wisc_bc_ContinuousVar.csv',header=TRUE, sep=",")
db = db[,-1]
db<-na.omit(db)
summary(db)
View(db)

train_index <- sample(nrow(db),as.integer(.70*nrow(db)))
train_data<-db[train_index,]
test_data<-db[-train_index,]

model <- neuralnet(diagnosis~.,data=train_data,hidden=5, threshold=0.01)
plotnet(model)

pred<-compute(model ,test_data[,-1])
ann<-c('B','M')[apply(pred$net.result,1,which.max)]

inc = (test_data$diagnosis != ann)
accuracy <-1 - sum(inc)/length(inc)
accuracy
