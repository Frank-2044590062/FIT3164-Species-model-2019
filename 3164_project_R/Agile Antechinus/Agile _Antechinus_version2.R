# this file implement data processing including data imputation.
# data1 file 
# 2/19/2019
# author: Ken Lin
# version: 2


rm(list = ls())
setwd("C:/Users/new/Desktop/3164_project_R/Agile Antechinus")
library('openxlsx')
data= read.xlsx('Agile Antechinus.xlsx', colNames = TRUE)


## store position

## data clean
## in this step I mainly force on clean the attribute that almost have same values for example, the beard-heat will
## never appear at sea, but their is still a sea which have this species, so I assume that, we can set that instance as
## low reliability and remove this column
data$COMMON_NME=NULL
data[,c(5,7)]=NULL

data = data[-(which(is.na(data$RECORD_TYPE))),]


data$bay=NULL
data[data$bua=='Y',1]= 'low reliability'
data$bua= NULL
data$island=NULL
data$island_marine=NULL
data[data$lga=='N',1] =  'low reliability'
data$lga=NULL
data$locality=NULL
data$mainland=NULL
data[data$named_natural_region=='Y',1]= 'low reliability'
data$named_natural_region=NULL
data[data$postcode=='N',1] = 'low reliability'
data$postcode=NULL

data$sea=NULL
data[data$wb_lake=='Y',1]= 'low reliability'
data[data$wb_lake_salt=='Y',1]= 'low reliability'
data$wb_lake=NULL
data$wb_lake_salt=NULL
data$wetland_swamp=NULL
#overview of data
summary(data)

# data computation. in these process, the relability is filled by a cluster.
# what we do is using a cluster find two groups of data, and then, based on the characteristic of data, we mannually give a
# high/low reiliablity to our data
library(stats)

# data computation
# select na
na_data = data[which(is.na(data$RELIABILITY_TXT)),]
complete_data = data[-which(is.na(data$RELIABILITY_TXT)),]
na_log = as.data.frame(na_data$LONGITUDEDD_NUM)
na_lan= as.data.frame(na_data$LATITUDEDD_NUM)
na_data$LATITUDEDD_NUM=NULL
na_data$LONGITUDEDD_NUM=NULL









# covert all the lable to digitals, for example, convert Yes to 1, No to 0 
# as the 
cluster_data= na_data[,2:24]


cluster_data[cluster_data$Point=='place',3]=0
cluster_data[cluster_data$Point=='airport',3]=1
cluster_data[cluster_data$Point=='mountain',3]=2
cluster_data[cluster_data$Point=='rail_station',3]=3


cluster_data[cluster_data$Line=='road',4]=0
cluster_data[cluster_data$Line=='watercourse_channel',5]=1
cluster_data[cluster_data$Line=='watercourse_stream',5]=2
cluster_data[cluster_data$Line=='state_border',5]=3
cluster_data[cluster_data$Line=='watercourse_river',5]=4
cluster_data[cluster_data$Line=='coast',5]=5
cluster_data[cluster_data$Line=='railway',5]=6
cluster_data[cluster_data$Line=='state_border',5]=7
cluster_data[cluster_data$Line=='rail_',5]=7


cluster_data[cluster_data$forest=='Y',7]= 1
cluster_data[cluster_data$forest=='N',7]= 0

cluster_data[cluster_data$public_land=='N',8]= 0
cluster_data[cluster_data$public_land=='Y',8]= 1

cluster_data[cluster_data$ridge=='N',9]= 0
cluster_data[cluster_data$ridge=='Y',9]= 1

cluster_data[cluster_data$vicgov_region=='Y',10]= 1
cluster_data[cluster_data$vicgov_region=='N',10]= 0


a=kmodes(cluster_data, 2)
cluster_label= as.data.frame(a$cluster)


cluster_table=  cbind(cluster_label,na_data)
cluster_table[cluster_table$`a$cluster`==1,2]='high reliability'
cluster_table[cluster_table$`a$cluster`==2,2]='low reliability'
cluster_table$`a$cluster`=NULL
cluster_table= cbind(cluster_table,na_lan)
cluster_table=cbind(cluster_table,na_log)
names(cluster_table)[28]= 'LATITUDEDD_NUM'
names(cluster_table)[29]= 'LONGITUDEDD_NUM'


write.csv(total_data, 'Agile Antechinus cluster data.csv')
detach(data)

