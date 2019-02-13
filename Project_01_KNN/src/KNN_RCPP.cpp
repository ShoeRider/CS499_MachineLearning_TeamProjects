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
NumericVector SortVector(NumericVector SortedTraining_inputs_ptr)
{
  NumericVector y = clone(SortedTraining_inputs_ptr);
  //Print_NumericVecort(y);
  std::sort(y.begin(), y.end());
  //Print_NumericVecort(y);
  return y;
}

int Transpose_NumericToEigen(NumericVector SortedTraining_inputs_ptr)
{
  NumericVector y = clone(SortedTraining_inputs_ptr);
  //Print_NumericVecort(y);
  std::sort(y.begin(), y.end());
  //Print_NumericVecort(y);
  return 0;
}



// [[Rcpp::export]]
int SortVector_Eigen(Eigen::VectorXd Eigen_Vector)
{

  //Print_NumericVecort(y);
  std::sort(Eigen_Vector.data(), Eigen_Vector.data()+Eigen_Vector.size());
  //Print_NumericVecort(y);
  return 0;
}




/*NN1toKmaxPredict takes a training data set and an entire test matrix,
then computes a matrix of k-nearest neighbor predictions,
for k=1 to max_neighbors, and for every test observation.
Parameters:
(
    n_train_observations, n_test_observations, n_features.
maximum number of neighbors: max_neighbors.
a matrix training data inputs (n_train_observations x n_features).
a vector of training data outputs (n_train_observations).
a test input matrix (n_test_observations x n_features).
a matrix of predictions for the test data (n_test_observations x max_neighbors),
which is where you need to store the result.

),

*/
//for some reason i couldnt figure out how to have (double*) 's in my CPP
// function, so i just used the NumericVector Object
// [[Rcpp::export]]
double NN1toKmaxPredict(
    double Training_inputs_ptr,
    double Training_lables_ptr,
    int N_TrainingObservations,
    int N_Features,
    int NRow,
    int NCol,
    int MaxNeighbors,
    double testing_inputs_ptr,
    double testing_Prediction_ptr,
    int N_TestObservations
)
{
  printf("%d",N_TrainingObservations);
  //
  //Sort based on N_Features into compartments, and have a separate function to
  //Create Array/ Sort of the closest elements



  //
  return 0;
}


// [[Rcpp::export]]
double printDouble(double* value,int N)
{
  printf("%lf",value);

  return 0;
}



 #endif // KNN_CPP
