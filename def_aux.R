#def_functions
#helper functions for deforestation program
#move these functions into C++...

get_dist=function(land,roads,splits=20)
{
  roads=seq_points(roads,splits) #sequence road points
  apply(rdist(land[,1:2],roads),1,min) #take min of each plot's distance to road points
}

seq_points=function(roads,splits)
{
  n=nrow(roads) #number of points to granulate btwn
  for(i in 1:(n-1))
  {
    x=seq(from=roads[i,1],to=roads[i+1,1],length.out=splits)
    y=seq(from=roads[i,2],to=roads[i+1,2],length.out=splits)
    temp=data.frame(x=x,y=y)
    roads<-rbind(roads,temp)
  }
  return(roads)
}



#outdated code.....

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


