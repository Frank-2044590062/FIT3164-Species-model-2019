# this file implement an roughly data clean processing and split data file by species (6 species)
# data1 file 
# 1/19/2019
# author: Ken Lin
# version: 1


## setting working directory, clean working platform, read data
rm(list = ls())
setwd("C:/Users/new/Desktop/3164_project_R")
library('openxlsx')
data= read.xlsx('complete_data.xlsx', colNames = TRUE)
species_label = unique(data$COMMON_NME)

## overview of data 
summary(data)
## remove irrelavant data
data[,c(1,2,3,5,6,7,9,10,11,15,16,17,20,21,22,23,24,25,26,27)]=NULL

for (i in species_label) {
  temp_data = data[data$COMMON_NME==i,]
  label= paste(i,'.xlsx',sep='')
  
  write.xlsx(temp_data,label)
  
}
