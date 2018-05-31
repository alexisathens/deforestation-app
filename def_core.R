#base deforestation app code
library(Rcpp)
library(microbenchmark)
library(fields) #for rdist
rm(list=ls(all=TRUE))
set.seed(1)

source('def_aux.R') #auxiliary R functions #deforestation-app/
sourceCpp('rcpp_functions.cpp') #auxiliary C++ functions #deforestation-app/
#source('test_funcs.R') #import test cases here of varying complexity..

#get from UI...
land=land #input$plot
roads=roads #df of user clicks
ntime=30 #input$years

#get distance to road points
land$dist.road=get_dist(land,roads)
arb.dist=0.50 #adjust w/r/t size of land?
#deforest automatically where road is constructed
land$dstatus=ifelse(land$dist.road<arb.dist,1,0) #where 1=deforested, 0=forested

for(i in 1:ntime)
{
  #get proportion of deforested neighbors every iteration
  land$def.nbors=def_nbors(land)
  
  #BART here
  
  #previously...
  tmp=exp(b0+b1*land$dist.road+b2*land$def.nbors)
  prob=tmp/(1+tmp)
  land$dstatus=rbinom(nrow(land),size=1,prob=prob)
}

###-------
#profile dist
land$dist.road
microbenchmark(
  euclidean_dist(land,roads), #my func
  get_dist(land,roads)
)

#quick def vis
land.m=df_to_matrix(land)
land.m



