library(grofit)
library(reshape2)
library(dplyr)

#read in raw data
data=read.table("082315_MIC.asc")

#extract the time information, remove the extraneous "s"
time=row.names(data)
time=as.numeric(sub("s", "",time))
row.names(data)=NULL

#define negative controls and subtract from dataset
neg=data[91:96]
neg=data.frame(rep(neg, 16))
data= data - neg

#make a time matrix based on number of observations and variables
n=nrow(data)
m=ncol(data)
time=matrix(rep(time, m), c(n, m))
time=t(time)

#transverse the data
data=t(data)

#read in platemap
platemap=read.csv("20150823_platemap.csv")

#combine data with platemap and remove well column
data=cbind(platemap, data)
data=data[,-1]

#run grofit
grofit(time, data, TRUE)
