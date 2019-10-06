# this file implement data processing including data imputation.
# data1 file 
# 2/19/2019
# author: Ken Lin
# version: 1


rm(list = ls())
setwd("C:/Users/new/Desktop/3164_project_R/southern brown tree frog")
library('openxlsx')
data= read.xlsx('complete_data.xlsx', colNames = TRUE)

data=data[data$COMMON_NME=='Southern Brown Tree Frog',]
data[,c(1,2,3,5,9,11,14,15,16,17,19,20:27)]=NULL
data$RELIABILITY_label= 0
data=na.omit(data)
data[data$RELIABILITY=='Acceptable' & data$RATING_INT==0 & data$RELIABILITY_TXT == 'High reliability',46] = 'High reliability'
data[data$RELIABILITY!='Acceptable' | data$RATING_INT!=0 | data$RELIABILITY_TXT != 'High reliability',46] = 'low reliability'
data[,c(1:4)]=NULL

## store position

## data clean
## in this step I mainly force on clean the attribute that almost have same values for example, the beard-heat will
## never appear at sea, but their is still a sea which have this species, so I assume that, we can set that instance as
## low reliability and remove this column


data$bay=NULL

data$island=NULL
data$island_marine=NULL
data$lga=NULL
data$locality=NULL
data$mainland=NULL
data$sea=NULL
data[data$wb_lak=='Y', 35]='low reliability'

data$wb_lake=NULL
data$wb_lake_salt=NULL
data$named_natural_region=NULL
data[data$wetland_swamp =='Y',32] = 'low reliablity'
data$wetland_swamp=NULL


data[data$postcode =='N',30]='low reliability'
data$postcode=NULL



