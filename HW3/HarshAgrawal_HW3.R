# Course      : CS 513
# First Name  : Harsh
# Last Name   : Agrawal
# CWID        : 10475285

rm(list=ls())
library(class)
setwd("C:/Users/Asus/Desktop/Github/MSCS/CS-513/HW3")

df = read.csv('breast-cancer-wisconsin.csv',header=TRUE, sep=",")
summary(df)

# Summary of F6 is missing as it has non numeric character
temp <- as.numeric(as.character(df$F6))
df$F6 <- temp
df <- na.omit(df)
summary(df)

# Now the summary of F6 is available, since the NA values of the row have been omitted.
# Class column has values 2 and 4, now converting that to the benign or malignant.

df$Class<- factor(df$Class , levels = c("2","4") , labels = c("benign","malignant"))
View(df)

#Dividing data into training and testing
trainIndex <- sample(1:nrow(df), round(0.7 * nrow(df))) 

##Normalizing the dataset features
features <- as.data.frame(lapply(df[,c(2,3,4,5,6,7,8,9,10)], function(x) {(x -min(x))/(max(x)-min(x))}))

# Taking the target column 
target = df['Class']

#train set
train <- features[trainIndex,] 
trainTarget <- target[trainIndex,]

##test set
test <- features[-trainIndex,] 
testTarget <- target[-trainIndex,]

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
#Building the KNN model for k = 3
clf <- knn(train,test,cl=trainTarget,k=3)

conf_matrix <- table(clf, testTarget)
print(conf_matrix)
accuracy(conf_matrix)

#Building the KNN model for k = 5
clf2 <- knn(train,test,cl=trainTarget,k=5)

conf_matrix2 <- table(clf2, testTarget)
print(conf_matrix2)
accuracy(conf_matrix2)

#Building the KNN model for k = 10
clf3 <- knn(train,test,cl=trainTarget,k=10)

conf_matrix3 <- table(clf3, testTarget)
print(conf_matrix3)
accuracy(conf_matrix3)