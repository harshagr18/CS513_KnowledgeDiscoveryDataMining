rm(list=ls())

library(class)
library(rpart)
library(rattle)

setwd("C:/Users/Asus/Desktop/Github/MSCS/CS-513/Project")

db = read.csv('train.csv',header=TRUE, sep=",")
testdb = read.csv('test.csv', header=TRUE, sep=",")
summary(db)

train = db[3:25]
test = testdb[3:25]

View(train)

CART_class <- rpart(satisfaction ~ ., data = train)
prediction <- predict(CART_class, test, type = "class")

conf_matrix <- table(prediction,test$satisfaction)
print(conf_matrix)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)

fancyRpartPlot(CART_class)
rm(list=ls())

#ANN

library(neuralnet)
library(NeuralNetTools)

setwd("C:/Users/Asus/Desktop/Github/MSCS/CS-513/Project")

db = read.csv('train.csv',header=TRUE, sep=",")
testdb = read.csv('test.csv', header=TRUE, sep=",")
summary(db)
train = db[3:25]
test = testdb[3:25]
View(db)

model <- neuralnet(satisfaction~.,data=db,hidden=5, threshold=0.01)
plotnet(model)

pred<-compute(model ,testdb[,-1])
ann<-c('B','M')[apply(pred$net.result,1,which.max)]

inc = (test_data$diagnosis != ann)
accuracy <-1 - sum(inc)/length(inc)
accuracy