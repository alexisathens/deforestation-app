#BART base
rm(list=ls())
setwd("Documents/UF/Valle Lab/Amazon Simulation/amazon_simulation/deforestation-app")
source("BART_aux.R")
set.seed(1)
library(invgamma)

#generate fake data
n=10 #number of observations
y=round(rnorm(n,mean=0,sd=2),2)
x1=round(runif(n),2)
x2=round(runif(n),2)
x3=round(runif(n),2)
dat <- data.frame(y=y,x1=x1,x2=x2,x3=x3,term.id=NA)
covs=c('x1','x2','x3')
dat

#create decision df
#function for number of internal nodes
dec.matrix=matrix(NA,7,3) #row for each decision node, corresponds to size of tree
dec.matrix[,1]=sample(covs,size=7,replace=T) #randomly sample from covs
dec.matrix[,2]=round(runif(7),2) #randomly generate thresholds
dec.matrix[,3]=1:7 #node id
colnames(dec.matrix)=c('cov','thresh','node.id') #dec.id

dec=as.data.frame(dec.matrix,stringsAsFactors = F) #create df with values
dec$thresh=as.numeric(dec$thresh)
dec$node.id=as.numeric(dec$node.id)
dec


#classify observations
dat$term.id=classify(dat,dec)
dat

#gibbs sampler
ngibbs=10000
mu=rep(1,8) #8 is number of terminal nodes b
store.param=matrix(NA,ngibbs,8)
param=list(mu,sigma=1)

#initialize
k<-2 #recommended param value
m<-1 #200 #number of trees
sigma.mu<-1/(2*k*sqrt(m))

for(i in 1:ngibbs)
{
  #param[[2]]=samp_sigma(param)
  for(k in 1:8)
  {
    param[[1]][k]=samp_mu(param,dat,k)
  }
  store.param[i,]=param[[1]]
}
