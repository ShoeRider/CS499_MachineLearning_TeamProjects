#ifndef KNN_CPP
#define KNN_CPP

//Implement KNN
//Implement KNN_1ToMax

#include "KNN.h"
//RCPP includes and R flags
////////////////////////////////////////////
#include <Rcpp.h>
using namespace Rcpp;
//using namespace Eigen;
//using namespace std;

////////////////////////////////////////////





// Note need this line for any cpp funtion we wish to use in R code, Dont remove
// [[Rcpp::export]]
double test_cpp(double x) {
  // Doubles the value at the memory location pointed to by x
  Eigen::MatrixXd m = Eigen::MatrixXd::Random(3,3);
  m = (m + Eigen::MatrixXd::Constant(3,3,1.2)) * 50;
  std::cout << "m =" << std::endl << m << std::endl;
  Eigen::VectorXd v(3);
  v << 1, 2, 3;
  std::cout << "m * v =" << std::endl << m * v << std::endl;
  return x;
}




//Also when using RCPP, you cant use pointers in parameters for function calls
// Note need this line for any cpp funtion we wish to use in R code, Dont remove
// [[Rcpp::export]]
double double_me_cpp(double x) {
  // Doubles the value at the memory location pointed to by x
  x = 2*x;
  return(x);
}

int Print_NumericVecort(NumericVector X)
{
  std::cout << "myvector contains:";
  for (NumericVector::iterator it=X.begin(); it!=X.end(); ++it)
    std::cout << ' ' << *it;
  std::cout << '\n';
  return 0;
}



// Note need this line for any cpp funtion we wish to use in R code, Dont remove
// [[Rcpp::export]]
NumericVector SortVector(NumericVector SortedTraining_inputs_ptr){
  NumericVector y = clone(SortedTraining_inputs_ptr);
  //Print_NumericVecort(y);
  std::sort(y.begin(), y.end());
  //Print_NumericVecort(y);
  return y;
}




// Note need this line for any cpp funtion we wish to use in R code, Dont remove
// [[Rcpp::export]]
double knn(
    double SortedTraining_inputs_ptr,
    double SortedTraining_lables_ptr,
    int NRow,
    int NCol,
    int MaxNeighbors,
    double testing_inputs_ptr,
    double testing_Prediction_ptr
  )
{
  return 0;
}


 #endif // KNN_CPP
