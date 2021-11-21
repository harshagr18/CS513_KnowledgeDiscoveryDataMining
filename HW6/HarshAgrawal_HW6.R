# Course      : CS 513
# First Name  : Harsh
# Last Name   : Agrawal
# CWID        : 10475285

rm(list=ls())

library(C50)

setwd("C:/Users/Asus/Desktop/Github/MSCS/CS-513/HW6")

db = read.csv('breast-cancer-wisconsin.csv',header=TRUE, sep=",")
summary(db)

db<-na.omit(db)

db$Class <- factor(db$Class, levels = c(2, 4), labels = c("benign", "malignant"))

View(db)

train_index <- sample(nrow(db),as.integer(.70*nrow(db)))
train_data<-db[train_index,]
test_data<-db[-train_index,]

c50<-C5.0(Class~.,train_data[,-1])
summary(c50)
plot(c50)

pred<-predict(c50,test_data[,-1],type="class") 
conf_matrix<-table(test_data[,11],pred)
conf_matrix

inc<-sum(test_data[,11]!=pred)
accuracy<- 1 - inc/length(test_data[,11])
accuracy

rm(list=ls())

# Random Forest
library('randomForest')

setwd("C:/Users/Asus/Desktop/Github/MSCS/CS-513/HW6")

db = read.csv('breast-cancer-wisconsin.csv',header=TRUE, sep=",")
summary(db)

db<-na.omit(db)

db$Class <- factor(db$Class, levels = c(2, 4), labels = c("benign", "malignant"))
db = db[-1]

View(db)

train_index <- sample(nrow(db),as.integer(.70*nrow(db)))
train_data<-db[train_index,]
test_data<-db[-train_index,]

rf<-randomForest(Class~.,data = train_data, importance = TRUE, ntree=1000)
varImpPlot(rf)
importance(rf)
#Hence F2 is the most important feature followed by F3 and then F7 in the order shown in the graph. F9 is the least important feature.

pred <-predict( rf ,test_data , type="class" )
inc <- sum(test_data[,10] != pred)
accuracy <- 1 - inc/length(test_data[,10])
accuracy

rm(list=ls())
