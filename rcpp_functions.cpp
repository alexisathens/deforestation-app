#include <Rcpp.h>
#include <iostream>

using namespace Rcpp;
//using namespace std;

// [[Rcpp::export]]
NumericMatrix defnbors(IntegerMatrix shape, IntegerMatrix deforest)
{
  int n = shape.ncol(); //shape is a square function
  
  //expand matrices
  /*
  int expshape[n+2][n+2]; //12 rows and cols for n=10, provides padding for matrix calculations
  int expdeforest[n+2][n+2];
  double updated[n+2][n+2];
  double new_deforest[n][n]; //return this matrix
  */
  
  NumericMatrix expshape(n+2,n+2);
  NumericMatrix expdeforest(n+2,n+2);
  NumericMatrix updated(n+2,n+2);
  NumericMatrix new_deforest(n,n);
  
  
  //initialize to 0
  for(int i = 0; i < n+2; ++i)
  {
    for(int j = 0; j < n+2; ++j)
    {
      expshape(i,j)=0;
      expdeforest(i,j)=0;
      updated(i,j)=0;
    }
  }
  
  //copy over data from orig to exp
  for(int i = 0; i < n; ++i)
  {
    for(int j = 0; j < n; ++j)
    {
      expshape(i+1,j+1)=shape(i,j);
      expdeforest(i+1,j+1)=deforest(i,j);
      updated(i+1,j+1)=deforest(i,j); //copy over current deforestation status into new matrix
    }
  }
  
  //calculate deforestation probabilities wrt neighbors
  double sum_nbors=0; //sum of neighboring deforestation statuses
  double nbors=0; //number of valid neighbors
  double avg_nbors=0; //sum/num of neighbors
  
  for(int i = 1; i < n+1; ++i)
  {
    for(int j = 1; j < n+1; ++j)
    {
      if(expshape(i,j) != 0) //if valid plot
      {
        if(expdeforest(i,j) != 1) //if not already deforested, find the average of neighbors
        {
          //get sum of neighbors
          sum_nbors = (expdeforest(i-1,j-1) + expdeforest(i,j-1) + expdeforest(i+1,j-1) + 
            expdeforest(i-1,j) + expdeforest(i+1,j) + expdeforest(i-1,j+1) + expdeforest(i,j+1) + 
            expdeforest(i+1,j+1));
          
          //calculate number of neighbors
          nbors = (expshape(i-1,j-1) + expshape(i,j-1) + expshape(i+1,j-1) +
          expshape(i-1,j) + expshape(i+1,j) + expshape(i-1,j+1) +
          expshape(i,j+1) + expshape(i+1,j+1));
          
          //avg_nbors = (double)sum_nbors / (double)nbors; //average of neighbors probs
          //try
          avg_nbors = sum_nbors / nbors; //average of neighbors probs
          
          updated(i,j) = avg_nbors;
        }
        else //already deforested
        { 
          updated(i,j) = 1;
        }
      }
    }
  }
  
  //resize updated from (n+2)x(n+2) to nxn
  for(int i = 0; i < n; ++i)
  {
    for(int j = 0; j < n; ++j)
    {
      new_deforest(i,j)=updated(i+1,j+1);
    }
  }
  
  return(new_deforest);
}
