
# this file implement modelling of common beard-heat.
# data1 file 
# 2/19/2019
# author: Ken Lin
# version: 2

rm(list = ls())
setwd("C:/Users/new/Desktop/3164_project_R/common Beard-heath")
library('openxlsx')
data= read.xlsx('Common Beard-heath.xlsx', colNames = TRUE)
