# Course      : CS 513
# First Name  : Harsh
# Last Name   : Agrawal
# CWID        : 10475285

rm(list=ls())

setwd("C:/Users/Asus/Desktop/Github/MSCS/CS-513/HW8")

db = read.csv('wisc_bc_ContinuousVar.csv',header=TRUE, sep=",")
db = db[,-1]
db<-na.omit(db)
summary(db)
View(db)


db2<-dist(db[,-1])
final<-hclust(db2)
plot(final)
db3<-cutree(final,2)
table(db3,db[,1])

rm(list=ls())


# KNN

setwd("C:/Users/Asus/Desktop/Github/MSCS/CS-513/HW8")

db = read.csv('wisc_bc_ContinuousVar.csv',header=TRUE, sep=",")
db = db[,-1]
db<-na.omit(db)
summary(db)
View(db)

kmodel<- kmeans(db[,-1],2,nstart = 10)
table(kmodel$cluster,db[,1])
