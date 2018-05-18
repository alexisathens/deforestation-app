#base deforestation app code
library(Rcpp)
rm(list=ls(all=TRUE))
set.seed(1)

source('deforestation-app/def_aux.R') #auxiliary R functions
sourceCpp('deforestation-app/rcpp_functions.cpp') #auxiliary C++ functions
#source('test_funcs.R') #import test cases here of varying complexity..

#get from UI...
land=land #input$plot
roads=roads #df of user clicks
ntime=30 #input$years

#get distance to road points
land$dist.road=euclidean_dist(land,roads)
#deforest plots near road
arb.dist=0.75
land$dstatus=ifelse(land$dist.road<arb.dist,1,0)

for(i in 1:ntime)
{
  #get proportion of deforested neighbors every iteration
  land$def.nbors=def_neighbors(land)
  
  #BART here?
  
  #previously...
  tmp=exp(b0+b1*land$dist.road+b2*land$def.nbors)
  prob=tmp/(1+tmp)
  land$dstatus=rbinom(nrow(land),size=1,prob=prob)
}

