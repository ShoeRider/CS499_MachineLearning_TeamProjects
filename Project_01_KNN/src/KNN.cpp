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
NumericVector SortVector(NumericVector SortedTraining_inputs_ptr)
{
  NumericVector y = clone(SortedTraining_inputs_ptr);
  //Print_NumericVecort(y);
  std::sort(y.begin(), y.end());
  //Print_NumericVecort(y);
  return y;
}


// [[Rcpp::export]]
NumericVector SortVector_Eigen(NumericVector *ToSort)
{
  Eigen::Map<Eigen::VectorXd> Eigen_Vector(ToSort,ToSort.size());
  //Print_NumericVecort(y);
  std::sort(Eigen_Vector.data(), Eigen_Vector.data()+Eigen_Vector.size());
  //Print_NumericVecort(y);
  return ToSort;
}




/*Predict_1ToMAX_KNearestNeighbors
  Parameters:
  (

  ),

*/
//Note: i am not sure if we are required to use (double pointers), or if we
//can use NumericVectors. I have an example of NumericVectors working in "SortVector_Eigen"
//But am unsure that is acceptable. Here is the version implemented in class:
int Predict_1ToMAX_KNearestNeighbors(
   //Training Points
    double * training_inputs_ptr,
    double * training_lables_ptr,
    int NRow,int NCol,
    int MaxNeighbors,
    //Test Points
    double * testing_inputs_ptr,
    double * testing_Prediction_ptr
  )
{
  //TODO:

  //Take input parameters and map them with the Eigen objects
  Eigen::VectorXd Difference_Vector(NCol);
  Eigen::VectorXd Sorted_Index_Vector(NCol);

  Eigen::Map<Eigen::MatrixXd> TrainingInput(training_inputs_ptr,NRow,NCol);
  Eigen::Map<Eigen::MatrixXd> TrainingLabel(training_lables_ptr,NCol);


  //Calculate Distance from TestPoint to Training data
  for(int i = 0; i < NRow; i++)
  {
    diff_vec = TrainingInput.row(i).transpose() - text_input_vec;
    Difference_Vector(i) = diff_vec.norm();
    Sorted_Index_Vector(i) = i;
  }


  //Sort the Distance Training Elements, so finding the nearest neighbors is quick
  std::sort(Eigen_Vector.data(), Eigen_Vector.data()+Eigen_Vector.size());

  //Sort the Distance Training Elements from the calculated distance array,
  //  This uses the included Lambda Function
  std::sort(Sorted_Index_Vector.data(),
            Sorted_Index_Vector.data() + Sorted_Index_Vector.size(),
            [&Difference_Vector](int lhs, int rhs)
            {
              return Difference_Vector(lhs) < Difference_Vector(rhs);
            }
          );

  double Label_Sum = 0.0;

  /*Loop from 1 to Max K NearestNeighbors


  */
  for(int i = 0; i < MaxNeighbors; i++)
  {
    //select element from accending Distance sorted list

    int NextNeighbor_VectorIndex = sorted_index_vec(i);
    //accumulate TotalY

    Label_Sum += train_label_pointer[NextNeighbor_VectorIndex];
    //find the Y hat value for each element in the array = TotalY / i
    testing_Prediction_ptr = Label_Sum / (k+1));
  }
  return 0;
}

int Predict_1



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
// [[Rcpp::export]]
double NN1toKmaxPredict(
  int MaxNeighbors,
  double Training_inputs_ptr,
  double Training_lables_ptr,
  int N_TrainingObservations,
  int N_Features,
  int NRow,
  int NCol,
  double testing_inputs_ptr,
  double testing_Prediction_ptr,
  int N_TestObservations,
  )
{
  return 0;
}


 #endif // KNN_CPP
