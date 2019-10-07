# this file implement modelling of common beard-heat.
# data1 file 
# 2/19/2019
# author: Ken Lin
# version: 2


rm(list = ls())
setwd("C:/Users/new/Desktop/3164_project_R/white-browed treecreeper")
library('openxlsx')
species_data= read.csv('White-browed Treecreeper cluster data.csv', header = TRUE)
species_data$X=NULL
species_data$lan=NULL
species_data$log=NULL
# generate pesudo absence data
data=read.xlsx('complete_data.xlsx')
data[,c(1,2,3,5,6,7,9,11,15,16,17,20,21,22,23,24,25,26,27)]=NULL
data[,5:7]=NULL
data$bay=NULL
data$bua=NULL
data$island=NULL
data$island_marine=NULL
data$lga=NULL
data$locality=NULL
data$mainland=NULL
data$sea=NULL
data$postode=NULL
data$ridge=NULL
data$wb_lake=NULL
data$wb_lake_salt=NULL
data$wetland_swamp=NULL

data$LATITUDEDD_NUM=NULL
data$PRIMARY_CDE=NULL

data = na.omit(data)
set.seed(28339797)
data = data[data$COMMON_NME!='White-browed Treecreeper',]
data$COMMON_NME=NULL
names(data)[1]=names(species_data)[1]
names(data)[12]=names(species_data)[12]
absence_data.row = sample(1:nrow(data), 0.99*nrow(data))  
absence_data= data[-absence_data.row,]

absence_data[,1]='low reliability'


data = rbind(species_data,absence_data)
#write.csv(data,'common beard-heath modelling data.csv')

#######################
#######################
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
rf_model$importance
rf_table = table(predicted = rf_test, actual = test$Reliability)
print(rf_table)

# naiv bayes

naiv.model=naiveBayes(Reliability~., data = train)
#  make the prediction for the test set
naiv.predict = predict(naiv.model, test)
# combine the predict result with actaul value
df1= as.data.frame(cbind(naiv.predict, test$Reliability))
# build the table to show the output
tn=table(predicted = naiv.predict, actual = test$Reliability)
print(tn)

#bagging
bag <- bagging(Reliability ~. , data = train, mfinal=5)
pred.bag <- predict.bagging(bag, test)
print(pred.bag$confusion)
#boosting
Boost <- boosting(Reliability ~. , data = train, mfinal=10)
pred.boost <- predict.boosting(Boost, newdata=test)
print(pred.boost$confusion)




train.row = sample(1:nrow(data), 0.8*nrow(data))  
train=data[train.row,]
test= data[-train.row,]

train$Reliability = factor(train$Reliability)
rf_model = randomForest(Reliability~., data= train,  ntree =500,type='classification')
rf_model$importance
detach(train)
attach(test)
detach(test)
#rf
rf_test= predict(rf_model,test)
df= as.data.frame(cbind(rf_test, test$Reliability))
rf_table = table(predicted = rf_test, actual = test$Reliability)
print(rf_table)


# naiv bayes

naiv.model=naiveBayes(Reliability~., data = train)
#  make the prediction for the test set
naiv.predict = predict(naiv.model, test)
# combine the predict result with actaul value
df1= as.data.frame(cbind(naiv.predict, test$Reliability))
# build the table to show the output
tn=table(predicted = naiv.predict, actual = test$Reliability)
print(tn)

#bagging
bag <- bagging(Reliability ~. , data = train, mfinal=5)
pred.bag <- predict.bagging(bag, test)
print(pred.bag$confusion)
#boosting
Boost <- boosting(Reliability ~. , data = train, mfinal=10)
pred.boost <- predict.boosting(Boost, newdata=test)
print(pred.boost$confusion)




train.row = sample(1:nrow(data), 0.9*nrow(data))  
train=data[train.row,]
test= data[-train.row,]

train$Reliability = factor(train$Reliability)
rf_model = randomForest(Reliability~., data= train,  ntree =500,type='classification')
rf_model$importance
detach(train)
attach(test)
detach(test)

rf_test= predict(rf_model,test)
df= as.data.frame(cbind(rf_test, test$Reliability))
rf_table = table(predicted = rf_test, actual = test$Reliability)
print(rf_table)


# naiv bayes

naiv.model=naiveBayes(Reliability~., data = train)
#  make the prediction for the test set
naiv.predict = predict(naiv.model, test)
# combine the predict result with actaul value
df1= as.data.frame(cbind(naiv.predict, test$Reliability))
# build the table to show the output
tn=table(predicted = naiv.predict, actual = test$Reliability)
print(tn)

#bagging
bag <- bagging(Reliability ~. , data = train, mfinal=5)
pred.bag <- predict.bagging(bag, test)
print(pred.bag$confusion)
#boosting
Boost <- boosting(Reliability ~. , data = train, mfinal=10)
pred.boost <- predict.boosting(Boost, newdata=test)
print(pred.boost$confusion)

