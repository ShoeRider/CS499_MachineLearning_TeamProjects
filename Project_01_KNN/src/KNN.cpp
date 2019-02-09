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



// Note need this line for any cpp funtion we wish to use in R code, Dont remove
// [[Rcpp::export]]
//Also when using RCPP, you cant use pointers in parameters for function calls
double double_me_cpp(double x) {
  // Doubles the value at the memory location pointed to by x
  x = 2*x;
  return(x);
}




int knn(
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
  Eigen::Map<Eigen::MatrixXd, > Training_input(training_inputs_ptr);

  Eigen::MatrixXd m = Eigen::MatrixXd::Random(3,3);
  return 0;
}


/*Takes:
  (

  ),
  Modifies the Vector at: testing_Prediction_ptr,
    selecting a classification for a single point
  Note: this function might be able to be made faster by segmenting points into a sorted 'bucket',
    By sorting once, we might see gains with Larger lists of training data...
*/
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
  //
  //Sort Training Elements, so finding the nearest neighbors is quick
  //


  //Loop from 1 to Max K NearestNeighbors
  //Could use OpenMP to make this component Parallel

  //omp_set_num_threads(2);
  //#pragma omp parallel private() shared()
  for(int i=0; i < MaxNeighbors; i++)
  {
    //select element from accending Distance sorted list

    //accumulate TotalY
    //find the Y hat value for each element in the array = TotalY / i
  }
  return 0;
}




/*Takes:
  (

  ),
  Modifies the Vector at: testing_Prediction_ptr,
    selecting a classification for a single point
  Note: this function might be able to be made faster by segmenting points into a sorted 'bucket',
    By sorting once, we might see gains with Larger lists of training data...
*/
int PredictTestArray_1ToMAX_KNearestNeighbors
  (
   //Training Points
    double * training_inputs_ptr,
    double * training_Lables,
    int NRow,int NCol,
    int MaxNeighbors,
    //Test Points
    double * testing_inputs_ptr,
    double * testing_Prediction_ptr,
    int NTest_Row,int NTest_Col
  )
 {
	//TODO:
    //Take input parameters and map them with the Eigen objects
    //Create a Sorted List for an individual element
	//for loop over the entire testing_Prediction_ptr Array
	return 0;
 }






 #endif // KNN_CPP
