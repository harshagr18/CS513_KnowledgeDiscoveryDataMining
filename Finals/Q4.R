# Course      : CS 513
# First Name  : Harsh
# Last Name   : Agrawal
# CWID        : 10475285

rm(list=ls())
setwd("C:/Users/Asus/Desktop/Github/CS513_KnowledgeDiscoveryDataMining/Finals")
df = read.csv('Data.csv',header=TRUE, sep=",")


df2<-hclust(dist(df[,-1]))

df3<-cutree(df2,2)
df4 = list(cluster = df3)
cluster = aggregate(df[,-1],df4,mean)

cluster

# b,c,d,f belong to cluster 1 and a,e,g belong to cluster 2
# (4.25,3.5,4.25) is the center of cluster 1
# (1.33,1.33,1.33) is the center of cluster 2
