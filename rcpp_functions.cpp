#include <Rcpp.h>
#include <iostream>
#include <cmath> //for abs()

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector def_nbors(DataFrame land) //get proportion of deforested neighbors for each plot
{
  //initialize vars
  int n=land.nrow();
  double def_nbors=0; //number of deforested neighbors
  double total_nbors=0; //number of valid neighbors
  NumericVector prop_nbors(n);
  //initialize to 0
  for(int i=0;i<n;++i)
  {
    prop_nbors[i]=0;
  }
  
  //get vectors from df
  IntegerVector x=land["x"];
  IntegerVector y=land["y"];
  IntegerVector dstatus=land["dstatus"];
  
  for(int i = 0; i < n; ++i) //iterate through plots
  {
    def_nbors=0;
    total_nbors=0;
    
    for(int j = 0; j < n; ++j) //check for neighbors
    {
      if((x[j]==(x[i]-1) && y[j]==(y[i]-1)) || 
         (x[j]==(x[i]-1) && y[j]==y[i]) ||
         (x[j]==(x[i]-1) && y[j]==(y[i]+1)) ||
         (x[j]==x[i] && y[j]==(y[i]-1)) ||
         (x[j]==x[i] && y[j]==(y[i]+1)) ||
         (x[j]==(x[i]+1) && y[j]==(y[i]-1)) ||
         (x[j]==(x[i]+1) && y[j]==y[i]) ||
         (x[j]==(x[i]+1) && y[j]==(y[i]+1)))
      {
        def_nbors+=dstatus[j];
        total_nbors++;
      }
    }
    prop_nbors[i]=def_nbors/total_nbors;
  }
  
  return(prop_nbors);
}
