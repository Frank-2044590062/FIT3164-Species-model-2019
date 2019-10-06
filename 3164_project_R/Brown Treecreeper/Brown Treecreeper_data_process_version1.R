# this file implement data processing including data imputation.
# data1 file 
# 2/19/2019
# author: Ken Lin
# version: 1


rm(list = ls())
setwd("C:/Users/new/Desktop/3164_project_R/Brown Treecreeper")
library('openxlsx')
data= read.xlsx('Brown Treecreeper.xlsx', colNames = TRUE)


## store position

## data clean
## in this step I mainly force on clean the attribute that almost have same values for example, the beard-heat will
## never appear at sea, but their is still a sea which have this species, so I assume that, we can set that instance as
## low reliability and remove this column
data$COMMON_NME=NULL
data[,c(5,7)]=NULL

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
data[data$bua=='Y',1]='low reliability'
data$bua  = NULL
data[data$named_natural_region=='Y',1]='low reliability'
data$named_natural_region=NULL
data[data$postcode=='N',1]='low reliability'
data$postcode=NULL

attach(data)
# covert all the lable to digitals, for example, convert Yes to 1, No to 0 
# as the 
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
cluster_table$`a$cluster`=NULL
cluster_table = cbind(cluster_table,log)
cluster_table=cbind(cluster_table,lan)
write.csv(cluster_table, 'common beard-heath cluster data.csv')
detach(data)

