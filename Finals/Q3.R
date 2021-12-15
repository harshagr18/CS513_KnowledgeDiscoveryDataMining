# Course      : CS 513
# First Name  : Harsh
# Last Name   : Agrawal
# CWID        : 10475285

rm(list=ls())
setwd("C:/Users/Asus/Desktop/Github/CS513_KnowledgeDiscoveryDataMining/Finals")
df = read.csv('Data.csv',header=TRUE, sep=",")


kmodel<- kmeans(df[,-1],2,nstart = 10)

print(kmodel$centers)

# b,c,d,f belong to cluster 1 and a,e,g belong to cluster 2
# (4.25,3.5,4.25) is the center of cluster 1
# (1.33,1.33,1.33) is the center of cluster 2
