# this file implement data processing including data imputation.
# data1 file 
# 2/19/2019
# author: Ken Lin
# version: 1


rm(list = ls())
setwd("C:/Users/new/Desktop/3164_project_R/Brown Treecreeper")
library('openxlsx')
data= read.xlsx('complete_data.xlsx', colNames = TRUE)

data=data[data$COMMON_NME=='Brown Treecreeper',]
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
data[data$bua=='Y',41]='low reliability'
data$bua  = NULL
data$island=NULL
data$island_marine=NULL
data$lga=NULL
data$locality=NULL
data$mainland=NULL
data$sea=NULL
data[data$wb_lak=='Y', 34]='low reliability'
data$wb_lake=NULL
data$wb_lake_salt=NULL
data[data$wetland_swamp =='Y',32] = 'low reliablity'
data$wetland_swamp=NULL
data[data$named_natural_region=='Y',31]='low reliablity'
data$named_natural_region=NULL
data[data$postcode =='N',30]='low reliability'
data$postcode=NULL
data$ridge=NULL
data$vicgov_region=NULL



#overview of data
summary(data)

# data computation. in these process, the relability is filled by a cluster.
# what we do is using a cluster find two groups of data, and then, based on the characteristic of data, we mannually give a
# high/low reiliablity to our data
log = data$LONGITUDEDD_NUM
lan= data$LATITUDEDD_NUM
data$LATITUDEDD_NUM=NULL
data$LONGITUDEDD_NUM=NULL
library(stats)
library(klaR)
# preliminary computation

attach(data)
# covert all the lable to digitals, for example, convert Yes to 1, No to 0 
# as the 
cluster_data= data[,1:24]

a=kmodes(cluster_data, 2)
cluster_label= as.data.frame(a$cluster)


cluster_table=  cbind(data,cluster_label)
cluster_table[cluster_table$`a$cluster`==1,26]='high reliability'
cluster_table[cluster_table$`a$cluster`==2,26]='low reliability'
cluster_table$RELIABILITY_label=NULL
names(cluster_table)[25]='Reliability'
cluster_table = cbind(cluster_table,log)
cluster_table=cbind(cluster_table,lan)
write.csv(cluster_table, 'Brown Treecreeper data cluster.csv')
detach(data)

