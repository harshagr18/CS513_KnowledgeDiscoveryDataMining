# Course      : CS 513
# First Name  : Harsh
# Last Name   : Agrawal
# CWID        : 10475285

rm(list=ls())
library(e1071)
#library(class) 
setwd("C:/Users/Asus/Desktop/Github/MSCS/CS-513/HW4")

db = read.csv('breast-cancer-wisconsin.csv',header=TRUE, sep=",")
summary(db)

db<-na.omit(db)

db$Class <- factor(db$Class, levels = c(2, 4), labels = c("benign", "malignant"))
db<- db[2:11]
class(db$Class)

View(db)

train_index <- sample(nrow(db),as.integer(.70*nrow(db)))
train_data<-db[train_index,]
test_data<-db[-train_index,]

model <- naiveBayes(Class~., data=train_data)
prediction <- predict(model, test_data)
conf_matrix <- table(predict_nb = prediction, class = test_data$Class)
print(conf_matrix)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)

rm(list=ls())