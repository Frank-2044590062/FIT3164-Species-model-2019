# this file implement modelling of common beard-heat.
# data1 file 
# 2/19/2019
# author: Ken Lin
# version: 2


rm(list = ls())
setwd("C:/Users/new/Desktop/3164_project_R/southern brown tree frog")
library('openxlsx')
data= read.csv('Southern Brown Tree Frog cluster.csv', header = TRUE)
#modelling and testing
train.row = sample(1:nrow(data), 0.7*nrow(data))  
train=data[train.row,]
test= data[-train.row,]

library(tree)
library(randomForest)
attach(train)
rf_model = randomForest(Reliability~., data= train,  ntree =500,type='classification')
detach(train)
attach(test)
detach(test)

rf_test= predict(rf_model,test)
df= as.data.frame(cbind(rf_test, test$RELIABILITY_TXT))

fff= tree(RELIABILITY_TXT~., train)
