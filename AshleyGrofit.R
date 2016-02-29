library(grofit)
library(reshape2)
library(dplyr)

#read in raw data
data=read.table("082315_MIC.asc")

#extract the time information, remove the extraneous "s" and re-append to the data
time=row.names(data)
time=as.numeric(sub("s", "",time))
data=cbind(time,data)
row.names(data)=NULL

#reshape the dataframe
data2=melt(data, id=("time"), variable.name="Well", value.name="OD590")

#read in plate map
platemap=read.csv("20150823_platemap.csv")

#combine plate map with data, for plotting with ggplot as per the Lenski lab protocol
plotting.data <- inner_join(data2, platemap, by="Well")

#reformat data for grofit
#make the time x experiment matrix, OD is values in cells
time.gf=t(data)
colnames(time.gf)=time.gf[1,]
time.gf=time.gf[-1,]
time.gf.v=as.vector(time.gf)

#make the "data" file which is the experiment information
exp.gf=platemap


gcFit(time.gf.v, exp.gf.v)


