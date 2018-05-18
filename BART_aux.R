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
assign_term <- function(dat,dec,this.id=1)
{
  if(floor(log2(this.id)) > floor(log2(max(dec$node.id)))) { #if beyond dec nodes, assign terminal node
    leaf.id=this.id-length(dec$node.id)
    return(leaf.id)
  } else if(dat[dec$cov[this.id]] > dec$thresh[this.id]) { #left child
    assign_term(dat,dec,2*this.id)
  } else { #right child
    assign_term(dat,dec,2*this.id+1)
  }
}
