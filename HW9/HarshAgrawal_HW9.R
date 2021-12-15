# Course      : CS 513
# First Name  : Harsh
# Last Name   : Agrawal
# CWID        : 10475285

rm(list=ls())
setwd("C:/Users/Asus/Desktop/Github/CS513_KnowledgeDiscoveryDataMining/HW9")

# Reading and removing NAs and iD column
db = read.csv('wisc_bc_ContinuousVar.csv',header=TRUE, sep=",")
db = db[,-1]
db<-na.omit(db)
summary(db)

#Splitting into training and testing
train_index <- sample(nrow(db),as.integer(.70*nrow(db)))
train_data<-db[train_index,]
test_data<-db[-train_index,]

#Factorizing
train_data$diagnosis <- factor(train_data$diagnosis)
test_data$diagnosis <- factor(test_data$diagnosis)

#SVM model building
library(e1071)
svm.model <- svm( diagnosis~ ., data = train_data  )
svm.pred <- predict(svm.model, test_data )

#Testing accuracy and printing model parameters
table(actual=test_data[,1],svm.pred )
SVM_wrong<- (test_data$diagnosis!=svm.pred)
rate<-sum(SVM_wrong)/length(SVM_wrong)
accuracy = 1-rate
accuracy
print(svm.model)
