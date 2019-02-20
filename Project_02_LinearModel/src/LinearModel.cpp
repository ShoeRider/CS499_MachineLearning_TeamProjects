#include <Eigen/Dense>
#include "LinearModel.h"
//LinearModel

//Takes a normalized DataSet
//
//Scalars
//and Bias
int LM_FindPrediction(
    const double * train_input_ptr, // n_observations x n_features
    const double * train_label_ptr, // n_observations
    const double * test_input_ptr, // n_features
    const int n_observations,
    const int n_features,
    const double * scalars,
    const double bias,
    double * test_predict_ptr // max_neighbors
)
{
  printf("TestOutput");
  Eigen::Map<Eigen::MatrixXd> train_inputs_mat((double *) train_input_ptr, n_observations, n_features);
  Eigen::Map<Eigen::VectorXd> test_input_vec((double *) train_label_ptr, n_features);
  printf("Textoutput\n");
  for(int i=0;i<n_observations;i++)
  {
    //test_predict_ptr[i] = scalars[i] * train_inputs_mat.row() + bias;
  }
  return 0;
}
