#BART auxiliary functions

#classify the data set into terminal nodes
classify <- function(dat,dec)
{
  term.id <- rep(0,nrow(dat))
  for(i in 1:nrow(dat))
  {
    term.id[i]=assign_term(as.vector(dat[i,]),dec)
  }
  return(term.id)
}

#assign each observation an appropriate terminal node
assign_term <- function(dat,dec,this.id=1) #modify so works for any shape/size binary tree
{
  if(floor(log2(this.id)) > floor(log2(max(dec$node.id)))) { #if beyond internal nodes, assign terminal node
    leaf.id=this.id-length(dec$node.id)
    return(leaf.id)
  } else if(dat[dec$cov[this.id]] > dec$thresh[this.id]) { #left child
    assign_term(dat,dec,2*this.id)
  } else { #right child
    assign_term(dat,dec,2*this.id+1)
  }
}

samp_mu <- function(param,dat,k)
{
  sigma=param[[2]]
  cond=dat$term.id==k
  L<-sum(cond) #number of observations in leaf
  nu<-sqrt(1/(L/sigma+1/sigma.mu)) #sd of normal-normal conjugation
  mean<-((nu/sigma)^2)*sum(dat[which(cond),"y"])
  return(rnorm(1,mean,nu))
}

samp_sigma <- function()
{
  rinvgamma(1,shape=3,rate=0.9)
}
