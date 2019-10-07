# this file implement modelling of common beard-heat.
# data1 file 
# 2/19/2019
# author: Ken Lin
# version: 2


rm(list = ls())
setwd("C:/Users/new/Desktop/3164_project_R/Brown Treecreeper")
library('openxlsx')
species_data= read.csv('Brown Treecreeper data.csv', header = TRUE)
species_data$X=NULL
species_data$lan=NULL
species_data$log=NULL
# generate pesudo absence data
data=read.xlsx('complete_data.xlsx')
data[,c(1,2,3,5,6,7,9,11,15,16,17,20,21,22,23,24,25,26,27)]=NULL

data[,c(6,8)]=NULL
data$LATITUDEDD_NUM=NULL
data$LONGITUDEDD_NUM=NULL
data$bay=NULL
data$island=NULL
data$island_marine=NULL
data$lga=NULL
data$locality=NULL
data$mainland=NULL
data$sea=NULL
data$wb_lake=NULL
data$wb_lake_salt=NULL
data$wetland_swamp=NULL
data$bua  = NULL
data$named_natural_region=NULL
data$postcode=NULL

data$RELIABILITY_TXT=NULL
data$Reliability='low reliability'
data$ridge=NULL
data$vicgov_region=NULL
data = na.omit(data)
set.seed(28339797)
data = data[data$COMMON_NME!='Brown Treecreeper' ,]
data$COMMON_NME=NULL
names(data)[9]= names(species_data)[9]
absence_data.row = sample(1:nrow(data), 0.85*nrow(data))  
absence_data= data[-absence_data.row,]
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
#randomforest
rf_model = randomForest(Reliability~., data= train,  ntree =500,type='classification')
detach(train)
attach(test)
detach(test)

rf_test= predict(rf_model,test)
df= as.data.frame(cbind(rf_test, test$Reliability))
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



train$Point=NULL
train$Line = NULL
train$forest=NULL
train$public_land=NULL
train$Anisotrophic_Heating_Ruggedbess.tif=NULL
train$ecoregion1750.tif = NULL

test$Point=NULL
test$Line = NULL
test$forest=NULL
test$public_land=NULL
test$Anisotrophic_Heating_Ruggedbess.tif=NULL
test$ecoregion1750.tif = NULL
attach(train)
rf_model = randomForest(Reliability~., data= train,  ntree =500,type='classification')
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


train$Point_Distance=NULL
train$Point = NULL
train$Line=NULL
train$Line_Distance=NULL
train$forest=NULL
train$public_land=NULL
train$Anisotrophic_Heating_Ruggedbess.tif = NULL
train$ecoregion1750.tif = NULL

test$Point_Distance=NULL
test$Point = NULL
test$Line=NULL
test$Line_Distance=NULL
test$forest=NULL
test$public_land=NULL
test$Anisotrophic_Heating_Ruggedbess.tif = NULL
test$ecoregion1750.tif = NULL
attach(train)
train$Reliability = factor(train$Reliability)
rf_model = randomForest(Reliability~., data= train,  ntree =500,type='classification')
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

train$Point_Distance=NULL
train$vegtpye3_4.tif = NULL
train$Point = NULL
train$Line = NULL
train$Line_Distance = NULL
train$forest = NULL
train$public_land=NULL
train$Anisotrophic_Heating_Ruggedbess.tif = NULL
train$ecoregion1750.tif=NULL

test$Point_Distance=NULL
test$vegtpye3_4.tif = NULL
test$Point = NULL
test$Line = NULL
test$Line_Distance = NULL
test$forest = NULL
test$public_land=NULL
test$Anisotrophic_Heating_Ruggedbess.tif = NULL
test$ecoregion1750.tif=NULL

attach(train)
train$Reliability = factor(train$Reliability)
rf_model = randomForest(Reliability~., data= train,  ntree =500,type='classification')
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


