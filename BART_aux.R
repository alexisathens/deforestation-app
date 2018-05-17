#BART auxiliary functions

#split data into appropriate terminal nodes
assign.term <- function(dat,dec,this.id=1)
{
  if(floor(log2(this.id)) > floor(log2(max(dec$node.id)))) { #if beyond dec nodes, assign terminal node
    leaf.id=this.id-length(dec$node.id) #this.id-7
    return(leaf.id)
  } else if(dat[dec$cov[this.id]] > dec$thresh[this.id]) { #move left
    assign.term(dat,dec,2*this.id)
  } else { #move right
    assign.term(dat,dec,2*this.id+1)
  }
}