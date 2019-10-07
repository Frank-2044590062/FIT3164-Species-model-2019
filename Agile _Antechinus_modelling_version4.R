# this file implement modelling of common beard-heat.
# data1 file 
# 2/19/2019
# author: Ken Lin
# version: 2


rm(list = ls())
setwd("C:/Users/new/Desktop/3164_project_R/Agile Antechinus")
library('openxlsx')
species_data= read.csv('Agile Antechinus cluster data.csv', header = TRUE)
species_data$X=NULL
species_data$LATITUDEDD_NUM=NULL
species_data$LONGITUDEDD_NUM=NULL
species_data[species_data$RELIABILITY_TXT =='High reliability',1] ='high reliability'
# generate pesudo absence data
data=read.xlsx('complete_data.xlsx')
data[,c(1,2,3,5,6,7,9,11,15,16,17,20,21,22,23,24,25,26,27)]=NULL





data$bay=NULL

data$bua= NULL
data$island=NULL
data$island_marine=NULL

data$lga=NULL
data$locality=NULL
data$mainland=NULL

data$named_natural_region=NULL

data$postcode=NULL

data$sea=NULL

data$wb_lake=NULL
data$wb_lake_salt=NULL
data$wetland_swamp=NULL
data$LATITUDEDD_NUM=NULL
data$LONGITUDEDD_NUM=NULL
data$LAT_LONG_ACCURACYDD_INT=NULL
data$PRIMARY_CDE=NULL
data = na.omit(data)
set.seed(28339797)
data = data[data$COMMON_NME!='Agile Antechinus',]
data$COMMON_NME=NULL

absence_data.row = sample(1:nrow(data), 0.8*nrow(data))  
absence_data= data[-absence_data.row,]
absence_data[,1]='low reliability'

names(species_data)[11]=names(absence_data)[11]

names(species_data)[12]=names(absence_data)[12]
data = rbind(species_data,absence_data)
write.csv(data,'Agile_Antechinus.csv')

#######################
#######################
#modelling and testing
unique(test$RELIABILITY_TXT)
train.row = sample(1:nrow(data), 0.7*nrow(data))  
train=data[train.row,]
test= data[-train.row,]

library(tree)
library(randomForest)
attach(train)
train$RELIABILITY_TXT = factor(train$RELIABILITY_TXT)
rf_model = randomForest(RELIABILITY_TXT~., data= train,  ntree =500,type='classification')
detach(train)
attach(test)
detach(test)
rf_model$importance
rf_test= predict(rf_model,test)
df= as.data.frame(cbind(rf_test, test$RELIABILITY_TXT))
rf_table = table(predicted = rf_test, actual = test$RELIABILITY_TXT)
print(rf_table)
rf_model$importance

# naiv bayes

naiv.model=naiveBayes(RELIABILITY_TXT~., data = train)
#  make the prediction for the test set
naiv.predict = predict(naiv.model, test)
# combine the predict result with actaul value
df1= as.data.frame(cbind(naiv.predict, test$RELIABILITY_TXT))
# build the table to show the output
tn=table(predicted = naiv.predict, actual = test$RELIABILITY_TXT)
print(tn)
#bagging
bag <- bagging(RELIABILITY_TXT ~. , data = train, mfinal=5)
pred.bag <- predict.bagging(bag, test)
print(pred.bag$confusion)
#boosting
Boost <- boosting(RELIABILITY_TXT ~. , data = train, mfinal=10)
pred.boost <- predict.boosting(Boost, newdata=test)
print(pred.boost$confusion)

train$Point_Distance=NULL
train$vegtpye3_4.tif = NULL
train$ridge = NULL
train$land_cov_use3.tif = NULL

test$Point_Distance=NULL
test$vegtpye3_4.tif = NULL
test$ridge = NULL
test$land_cov_use3.tif = NULL
#rf
rf_model = randomForest(RELIABILITY_TXT~., data= train,  ntree =500,type='classification')
rf_test= predict(rf_model,test)
df= as.data.frame(cbind(rf_test, test$RELIABILITY_TXT))
rf_table = table(predicted = rf_test, actual = test$RELIABILITY_TXT)
print(rf_table)

# naiv bayes

naiv.model=naiveBayes(RELIABILITY_TXT~., data = train)
#  make the prediction for the test set
naiv.predict = predict(naiv.model, test)
# combine the predict result with actaul value
df1= as.data.frame(cbind(naiv.predict, test$RELIABILITY_TXT))
# build the table to show the output
tn=table(predicted = naiv.predict, actual = test$RELIABILITY_TXT)
print(tn)
#bagging
bag <- bagging(RELIABILITY_TXT ~. , data = train, mfinal=5)
pred.bag <- predict.bagging(bag, test)
print(pred.bag$confusion)
#boosting
Boost <- boosting(RELIABILITY_TXT ~. , data = train, mfinal=10)
pred.boost <- predict.boosting(Boost, newdata=test)
print(pred.boost$confusion)
#80%
train.row = sample(1:nrow(data), 0.8*nrow(data))  
train=data[train.row,]
test= data[-train.row,]
#rf
rf_model = randomForest(RELIABILITY_TXT~., data= train,  ntree =500,type='classification')
rf_test= predict(rf_model,test)
df= as.data.frame(cbind(rf_test, test$RELIABILITY_TXT))
rf_table = table(predicted = rf_test, actual = test$RELIABILITY_TXT)
print(rf_table)

# naiv bayes

naiv.model=naiveBayes(RELIABILITY_TXT~., data = train)
#  make the prediction for the test set
naiv.predict = predict(naiv.model, test)
# combine the predict result with actaul value
df1= as.data.frame(cbind(naiv.predict, test$RELIABILITY_TXT))
# build the table to show the output
tn=table(predicted = naiv.predict, actual = test$RELIABILITY_TXT)
print(tn)
#bagging
bag <- bagging(RELIABILITY_TXT ~. , data = train, mfinal=5)
pred.bag <- predict.bagging(bag, test)
print(pred.bag$confusion)
#boosting
Boost <- boosting(RELIABILITY_TXT ~. , data = train, mfinal=10)
pred.boost <- predict.boosting(Boost, newdata=test)
print(pred.boost$confusion)

train$Point_Distance=NULL
train$vegtpye3_4.tif = NULL
train$land_cov_use3.tif = NULL
train$Point = NULL
train$ridge = NULL

test$Point_Distance=NULL
test$vegtpye3_4.tif = NULL
test$land_cov_use3.tif = NULL
test$Point = NULL
test$ridge = NULL

library(tree)
library(randomForest)
attach(train)
train$RELIABILITY_TXT = factor(train$RELIABILITY_TXT)
rf_model = randomForest(RELIABILITY_TXT~., data= train,  ntree =500,type='classification')
rf_model$importance
detach(train)
attach(test)
detach(test)

rf_test= predict(rf_model,test)
df= as.data.frame(cbind(rf_test, test$RELIABILITY_TXT))
rf_table = table(predicted = rf_test, actual = test$RELIABILITY_TXT)
print(rf_table)


# naiv bayes

naiv.model=naiveBayes(RELIABILITY_TXT~., data = train)
#  make the prediction for the test set
naiv.predict = predict(naiv.model, test)
# combine the predict result with actaul value
df1= as.data.frame(cbind(naiv.predict, test$RELIABILITY_TXT))
# build the table to show the output
tn=table(predicted = naiv.predict, actual = test$RELIABILITY_TXT)
print(tn)

#bagging
bag <- bagging(RELIABILITY_TXT ~. , data = train, mfinal=5)
pred.bag <- predict.bagging(bag, test)
print(pred.bag$confusion)
#boosting
Boost <- boosting(RELIABILITY_TXT ~. , data = train, mfinal=10)
pred.boost <- predict.boosting(Boost, newdata=test)
print(pred.boost$confusion)

train.row = sample(1:nrow(data), 0.9*nrow(data))  
train=data[train.row,]
test= data[-train.row,]

rf_model = randomForest(RELIABILITY_TXT~., data= train,  ntree =500,type='classification')
rf_test= predict(rf_model,test)
df= as.data.frame(cbind(rf_test, test$RELIABILITY_TXT))
rf_table = table(predicted = rf_test, actual = test$RELIABILITY_TXT)
print(rf_table)

# naiv bayes

naiv.model=naiveBayes(RELIABILITY_TXT~., data = train)
#  make the prediction for the test set
naiv.predict = predict(naiv.model, test)
# combine the predict result with actaul value
df1= as.data.frame(cbind(naiv.predict, test$RELIABILITY_TXT))
# build the table to show the output
tn=table(predicted = naiv.predict, actual = test$RELIABILITY_TXT)
print(tn)
#bagging
bag <- bagging(RELIABILITY_TXT ~. , data = train, mfinal=5)
pred.bag <- predict.bagging(bag, test)
print(pred.bag$confusion)
#boosting
Boost <- boosting(RELIABILITY_TXT ~. , data = train, mfinal=10)
pred.boost <- predict.boosting(Boost, newdata=test)
print(pred.boost$confusion)


train$Point_Distance=NULL
train$vegtpye3_4.tif = NULL
train$ridge = NULL

test$Point_Distance=NULL
test$vegtpye3_4.tif = NULL
test$ridge = NULL

library(tree)
library(randomForest)
attach(train)
train$RELIABILITY_TXT = factor(train$RELIABILITY_TXT)
rf_model = randomForest(RELIABILITY_TXT~., data= train,  ntree =500,type='classification')
detach(train)
attach(test)
detach(test)

rf_test= predict(rf_model,test)
df= as.data.frame(cbind(rf_test, test$RELIABILITY_TXT))
rf_table = table(predicted = rf_test, actual = test$RELIABILITY_TXT)
print(rf_table)


# naiv bayes

naiv.model=naiveBayes(RELIABILITY_TXT~., data = train)
#  make the prediction for the test set
naiv.predict = predict(naiv.model, test)
# combine the predict result with actaul value
df1= as.data.frame(cbind(naiv.predict, test$RELIABILITY_TXT))
# build the table to show the output
tn=table(predicted = naiv.predict, actual = test$RELIABILITY_TXT)
print(tn)
#bagging
bag <- bagging(RELIABILITY_TXT ~. , data = train, mfinal=5)
pred.bag <- predict.bagging(bag, test)
print(pred.bag$confusion)
#boosting
Boost <- boosting(RELIABILITY_TXT ~. , data = train, mfinal=10)
pred.boost <- predict.boosting(Boost, newdata=test)
print(pred.boost$confusion)

