# Course      : CS 513
# First Name  : Harsh
# Last Name   : Agrawal
# CWID        : 10475285

rm(list=ls())

library(class)
library(rpart)
library(rattle)

setwd("C:/Users/Asus/Desktop/Github/MSCS/CS-513/HW5")

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


CART_class <- rpart(Class ~ ., data = train_data)
prediction <- predict(CART_class, test_data, type = "class")

conf_matrix <- table(prediction,test_data$Class)
print(conf_matrix)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)

fancyRpartPlot(CART_class)
rm(list=ls())