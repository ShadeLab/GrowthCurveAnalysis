library(grofit)
library(reshape2)
library(dplyr)
library(ggplot2)

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

#transpose the data
data=t(data)

#read in platemap
platemap=read.csv("20150823_platemap.csv")

#combine data with platemap and remove well column
data=cbind(platemap, data)

#Well's were labeled twice, so redundant naming was removed
data=data[,-1]

#OPTION: run all of grofit
results=grofit(time, data)

#OPTION: only run table 
results=gcFit(time, data)

#make a data frame of the results
results=data.frame(read.table(results), header=TRUE)

#extract maximum growth parameter from dataset
mu=results$mu.spline

#add information to mu
mu=cbind(platemap, mu)

#add replicate number to data
reps=data.frame(read.table("Replicates.txt", header=TRUE))
mu=cbind(mu, reps)

#remove control values with replicate numbers
normalization=subset(mu, Concentration %in% 0)

#remove unimportant columns
normalization=normalization[,-1]
normalization=normalization[,-2]
normalization=normalization[,-2]

#add negatives back
mu=inner_join(mu, normalization, by = c("Strain", "Replicate"))

#normalize data
norm=mu$mu.x/mu$mu.y
norm=data.frame(norm)
#add information to normalized data
mu=cbind(mu, norm)

#group data
grouped=group_by(mu, Strain, Concentration)

#Calculate average and stdev
stats=summarise(grouped, Average=mean(norm), StDev=sd(norm))

#plot data
ggplot(data=stats, aes(x=Concentration, y=Average)) +
  geom_line(size=1.5) +
  geom_point(aes(color=Concentration), size=2.5) +
  geom_point(shape=1, size=2.5, color="black") +
  scale_color_gradientn(colors=rainbow(6)) +
  facet_wrap(~Strain)

#Get OD590 maximum data (A)
A=results$A.spline
A=cbind(platemap, A)

#add replicate number to data
reps=data.frame(read.table("Replicates.txt", header=TRUE))
A=cbind(A, reps)

#remove control values with replicate numbers
normalization=subset(A, Concentration %in% 0)

#remove unimportant columns
normalization=normalization[,-1]
normalization=normalization[,-2]
normalization=normalization[,-2]

#add negatives back
A=inner_join(A, normalization, by = c("Strain", "Replicate"))

#normalize data
norm=A$A.x/A$A.y

#add information to normalized data
A=cbind(A, norm)

#group data
grouped=group_by(A, Strain, Concentration)

#Calculate average and stdev
stats=summarise(grouped, Average=mean(norm), StDev=sd(norm))

#plot data
ggplot(data=stats, aes(x=Concentration, y=Average)) +
  geom_line(size=1.5) +
  geom_point(aes(color=Concentration), size=2.5) +
  geom_point(shape=1, size=2.5, color="black") +
  scale_color_gradientn(colors=rainbow(6)) +
  facet_wrap(~Strain)

#extract time to exponential growth parameter (lambda) from dataset
lambda=results$lambda.spline

#add information to mu
lambda=cbind(platemap, lambda)

#add replicate number to data
reps=data.frame(read.table("Replicates.txt", header=TRUE))
lambda=cbind(lambda, reps)

#remove control values with replicate numbers
normalization=subset(lambda, Concentration %in% 0)

#remove unimportant columns
normalization=normalization[,-1]
normalization=normalization[,-2]
normalization=normalization[,-2]

#add negatives back
lambda=inner_join(lambda, normalization, by = c("Strain", "Replicate"))

#normalize data
norm=lambda$lambda.x/lambda$lambda.y

#add information to normalized data
lambda=cbind(lambda, norm)

#group data
grouped=group_by(lambda, Strain, Concentration)

#Calculate average and stdev
stats=summarise(grouped, Average=mean(norm), StDev=sd(norm))

#plot data
ggplot(data=stats, aes(x=Concentration, y=Average)) +
  geom_line(size=1.5) +
  geom_point(aes(color=Concentration), size=2.5) +
  geom_point(shape=1, size=2.5, color="black") +
  scale_color_gradientn(colors=rainbow(6)) +
  facet_wrap(~Strain)

