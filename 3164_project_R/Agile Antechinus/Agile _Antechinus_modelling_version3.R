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
write.csv(data,'common beard-heath modelling data.csv')

#######################
#######################
#modelling and testing
train.row = sample(1:nrow(data), 0.8*nrow(data))  
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

rf_test= predict(rf_model,test)
df= as.data.frame(cbind(rf_test, test$RELIABILITY_TXT))

fff= tree(RELIABILITY_TXT~., train)
