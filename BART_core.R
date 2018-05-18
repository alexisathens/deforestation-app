#BART base
rm(list=ls())
source("deforestation-app/BART_aux.R")
set.seed(1)

#generate fake data
n=10
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

#gibbs
ngibbs=1000
mu1=mu2=mu3=1
store.param=matrix(NA,ngibbs,3) #recover mu_1 mu_2 and mu_3
param=list(mu1=mu1,mu2=mu2,mu3=mu3)

for(i in 1:ngibbs)
{
  param$mu1=samp.mu(param)
  param$mu1=samp.mu(param)
  param$mu1=samp.mu(param)
}
