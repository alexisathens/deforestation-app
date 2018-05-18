#def_functions
#helper functions for deforestation program
#move these functions into C++...

#euclidean distance between plots of land (x,y) and road coords
euclidean_dist=function(land,roads)
{
  roads=draw_pts(roads) #make lines (finer pts) btwn individual points
  
  dist <- rep(0,nrow(land))
  for(i in 1:nrow(land))
  {
    x2=(land$x[i]-roads$x)^2
    y2=(land$y[i]-roads$y)^2
    euc.dist=sqrt(x2+y2)
    dist[i]=min(euc.dist)
  }
  return(dist)
}

#calls recursive function to granulate road coords
draw_pts=function(roads)
{
  for(i in 1:(nrow(roads)-1)) #number of road coords (user clicks)
  {
    new.pts=midpoint(roads[i,],roads[i+1,])
    
    roads=rbind(roads,new.pts) #remove duplicates? only duplicates are user selected pts
  }
  return(roads)
}

#recursive function that continues taking midpoint between given points until threshold distance is reached
midpoint=function(pt1,pt2,pts=data.frame(x=pt1$x,y=pt1$y),thresh=1.0)
{
  euclidean.dist=sqrt((pt1$x-pt2$x)^2+(pt1$y-pt2$y)^2)
  
  if(euclidean.dist<thresh){ #new point
    if(!(pt1$x %in% pts$x)){ #if pt1 not already included in df, append
      pts=rbind(pts,data.frame(x=pt1$x,y=pt1$y))
    }
    if(!(pt2$x %in% pts$x)){ #if pt2 not already included in df, append
      pts=rbind(pts,data.frame(x=pt2$x,y=pt2$y))
    }
    return(pts)
  } else { #get midpoint
    mid.x=(pt1$x+pt2$x)/2
    mid.y=(pt1$y+pt2$y)/2
    mid=data.frame(x=mid.x,y=mid.y)
    pts=midpoint(pt1,mid,pts=pts)
    pts=midpoint(pt2,mid,pts=pts)
  }
  return(pts)
}

#intermediary function for determining proportion of deforested neighbors
def_neighbors=function(land)
{
  land.m=df_to_matrix(land)
  new.land.m=def_nbors(land.m)
  nbors=matrix_to_vec(new.land.m)
  return(nbors)
}

#converter function to be able to index particular plots rather than searching dataframe
#create matrix holding deforestation statuses of each plot
df_to_matrix=function(land)
{
  #create land matrix
  #-1 if off map, 0 if on map & forested, 1 if on map & deforested
  n=max(land$x,land$y)
  land.m=matrix(data=-1,nrow=n,ncol=n) #-1 off map, on map otherwise
  for(i in 1:nrow(land)) #mark as de/forested
  {
    land.m[land$x[i],land$y[i]]=ifelse(land$dstatus[i]==1,1,0)
  }
  return(land.m)
}

#convert matrix to vector to append to df
matrix_to_vec=function(land.m)
{
  #assuming for now that land df will be constructed the same as by the expand.grid function
  #i.e., the order for the x,y coords will be of the same construction
  land.v <- as.vector(land.m)
  nbors <- land.v[which(land.v!=-1)] #return value for plots that are "on" map only
  return(nbors)
}
