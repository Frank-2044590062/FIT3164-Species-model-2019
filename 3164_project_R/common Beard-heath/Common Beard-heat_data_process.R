# this file implement modelling of common beard-heat.
# data1 file 
# 2/19/2019
# author: Ken Lin
# version: 1


rm(list = ls())
setwd("C:/Users/new/Desktop/3164_project_R")
library('openxlsx')
data= read.xlsx('Common Beard-heath.xlsx', colNames = TRUE)


## store position

## data clean
data$COMMON_NME=NULL
data[,5:7]=NULL
log = data$LONGITUDEDD_NUM
lan= data$LATITUDEDD_NUM
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
#overview of data
summary(data)

# data computation. in these process, the relability by a cluster.
library(stats)

# preliminary computation
data[data$bua=='Y',1]='low reliability'
data$bua  = NULL
data[data$named_natural_region=='Y',1]='low reliability'
data$named_natural_region=NULL
data[data$postcode=='N',1]='low reliability'
data$postcode=NULL

attach(data)
cluster_data= data[,2:24]
cluster_data[cluster_data$SAMPLING_METHOD_DESC =='Quadrat',1]=0
cluster_data[cluster_data$SAMPLING_METHOD_DESC =='Species List for Defined Area',1]=1
cluster_data[cluster_data$SAMPLING_METHOD_DESC =='Incidental',1]=2
cluster_data[cluster_data$SAMPLING_METHOD_DESC =='Monitoring',1]=3
cluster_data[cluster_data$SAMPLING_METHOD_DESC =='Specimen',1]=4

cluster_data[cluster_data$Point=='place',2]=0
cluster_data[cluster_data$Point=='airport',2]=1
cluster_data[cluster_data$Point=='mountain',2]=2
cluster_data[cluster_data$Point=='rail_station',2]=3


cluster_data[cluster_data$Line=='road',4]=0
cluster_data[cluster_data$Line=='watercourse_channel',4]=1
cluster_data[cluster_data$Line=='watercourse_stream',4]=2
cluster_data[cluster_data$Line=='state_border',4]=3
cluster_data[cluster_data$Line=='watercourse_river',4]=4
cluster_data[cluster_data$Line=='coast',4]=5
cluster_data[cluster_data$Line=='railway',4]=6

cluster_data[cluster_data$forest=='Y',6]= 1
cluster_data[cluster_data$forest=='N',6]= 0

cluster_data[cluster_data$public_land=='N',7]= 0
cluster_data[cluster_data$public_land=='Y',7]= 1

cluster_data[cluster_data$ridge=='N',8]= 0
cluster_data[cluster_data$forest=='Y',8]= 1

cluster_data[cluster_data$vicgov_region=='Y',9]= 1
cluster_data[cluster_data$vicgov_region=='N',9]= 0


a=kmodes(cluster_data, 2)
cluster_label= as.data.frame(a$cluster)

cluster_table=  cbind(cluster_label,data)
cluster_table[cluster_table$`a$cluster`==1,2]='high reliability'
cluster_table[cluster_table$`a$cluster`==2,2]='low reliability'

write.csv(cluster_table, 'common beard-heath cluster data.csv')
detach(data)
