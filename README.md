# May12_Datathon_PublicHealth
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function library(plyr)
library(dplyr)
library(reshape2)
setwd("/u7/x554zhang/Desktop")
NYC311=read.csv('311_service_requests.csv',header=T)
#Clean data and make complaints uniform
levels(NYC311$complaint_type)
NYC311$complaint_type = tolower(NYC311$complaint_type)
NYC311$complaint_type = gsub('s$','',NYC311$complaint_type)
NYC311$incident_zip=gsub('-[[:digit:]]{4}$','',NYC311$incident_zip)
idx=grepl('[[:digit:]]{5}',NYC311$incident_zip)
NYC311clean=NYC311[idx,]
NYC311byZip = ddply(NYC311clean, .(incident_zip,complaint_type), count)
#Error: could not find function "ddply"
library(plyr)
library(tidyr)
raw=spread(NYC311byZip,complaint_type,n)
raw[is.na(raw)]=0
counts=which(colSums(raw[,-1])<10)
zipcodes=raw[,1]
raw=raw[,-1]
raw=raw[,-counts]
processed=scale(raw,center=T,scale=T)
library(psych)
pca=principal(processed,nfactor=6,covar=F)
